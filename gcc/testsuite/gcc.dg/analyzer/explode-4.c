/* Regression test for state explosion seen whilst implementing
   PR analyzer/110426, involving an explosion in the number of
   conjured_svalues whilst handling a long chain of external
   function calls.  */

/* { dg-additional-options "-Wno-implicit-function-declaration -Wno-int-conversion -Wno-analyzer-too-complex -Wno-analyzer-symbol-too-complex" } */

#define NULL ((void *)0)
typedef unsigned char uint8_t;
typedef unsigned short int uint16_t;
typedef unsigned int uint32_t;
typedef struct Error Error;
typedef struct QEMUTimer QEMUTimer;
enum libusb_class_code {
  LIBUSB_CLASS_PER_INTERFACE = 0x00,
};
struct libusb_device_descriptor {};
struct libusb_endpoint_descriptor {
  uint8_t bEndpointAddress;
  uint8_t bmAttributes;
};
struct libusb_interface_descriptor {
  uint8_t bNumEndpoints;
  const struct libusb_endpoint_descriptor *endpoint;
};
struct libusb_interface {
  const struct libusb_interface_descriptor *altsetting;
};
struct libusb_config_descriptor {
  uint8_t bNumInterfaces;
  const struct libusb_interface *interface;
};
typedef struct libusb_context libusb_context;
typedef struct libusb_device libusb_device;
typedef struct libusb_device_handle libusb_device_handle;
enum libusb_speed {
  LIBUSB_SUCCESS = 0,
  LIBUSB_TRANSFER_TYPE_BULK = 2U,
};
typedef struct USBDevice USBDevice;
typedef struct USBHostDevice USBHostDevice;
struct USBAutoFilter {
  uint32_t bus_num;
};
struct USBHostDevice {
  struct USBAutoFilter match;
  struct USBHostDevice *tqe_next;
  int bus_num;
  int addr;
  char port[16];
  libusb_device *dev;
  libusb_device_handle *dh;
  struct libusb_device_descriptor ddesc;
};
static union { struct USBHostDevice *tqh_first; } hostdevs = {};
static libusb_context *ctx;
static void usb_host_ep_update(USBHostDevice *s) {
  static const char *tname[] = {};
  USBDevice *udev = USB_DEVICE(s);
  struct libusb_config_descriptor *conf;
  const struct libusb_interface_descriptor *intf;
  const struct libusb_endpoint_descriptor *endp;
  struct libusb_ss_endpoint_companion_descriptor *endp_ss_comp;
  uint8_t devep, type;
  int ep;
  int rc, i, e;
  usb_ep_reset(udev);
  rc = libusb_get_active_config_descriptor(s->dev, &conf);
  for (i = 0; i < conf->bNumInterfaces; i++) {
    intf = &conf->interface[i].altsetting[0];
    trace_usb_host_parse_interface();
    for (e = 0; e < intf->bNumEndpoints; e++) {
      endp = &intf->endpoint[e];
      devep = endp->bEndpointAddress;
      ep = devep & 0xf;
      type = endp->bmAttributes & 0x3;
      if (usb_ep_get_type(udev) != 255) {
        trace_usb_host_parse_error();
      }
      trace_usb_host_parse_endpoint(s->bus_num, s->addr);
      usb_ep_set_max_packet_size(udev);
      usb_ep_set_type(udev);
      usb_ep_set_ifnum(udev);
      usb_ep_set_halted(udev);
      if (type == LIBUSB_TRANSFER_TYPE_BULK &&
          libusb_get_ss_endpoint_companion_descriptor(
              ctx, endp, &endp_ss_comp) == LIBUSB_SUCCESS) {
        usb_ep_set_max_streams(udev);
        libusb_free_ss_endpoint_companion_descriptor(endp_ss_comp);
      }
    }
  }
}
static int usb_host_open(USBHostDevice *s, libusb_device *dev, int hostfd) {
  USBDevice *udev = USB_DEVICE(s);
  int bus_num = 0;
  int addr = 0;
  Error *local_err = NULL;
  if (dev) {
    bus_num = libusb_get_bus_number(dev);
    addr = libusb_get_device_address(dev);
    trace_usb_host_open_started(bus_num, addr);
    libusb_open(dev, &s->dh);
    trace_usb_host_open_hostfd(hostfd);
    libusb_wrap_sys_device(ctx, &s->dh);
    dev = libusb_get_device(s->dh);
    bus_num = libusb_get_bus_number(dev);
    addr = libusb_get_device_address(dev);
  }
  usb_host_detach_kernel(s);
  libusb_get_device_descriptor(dev, &s->ddesc);
  usb_host_get_port(s->dev, s->port, sizeof(s->port));
  usb_ep_init(udev);
  usb_host_ep_update(s);
  libusb_get_device_speed(dev);
  usb_device_attach(udev, &local_err);
  if (local_err) {
    error_report_err(local_err);
    goto fail;
  }
  return 0;
fail:
  trace_usb_host_open_failure();
  if (s->dh != NULL) {
    usb_host_release_interfaces(s);
    libusb_reset_device(s->dh);
    usb_host_attach_kernel(s);
  }
}
static QEMUTimer *usb_auto_timer;
static void usb_host_vm_state(void *unused, _Bool running) {}
static void usb_host_auto_check(void *unused) {
  struct USBHostDevice *s;
  struct USBAutoFilter *f;
  libusb_device **devs = NULL;
  struct libusb_device_descriptor ddesc;
  int i, n;
  if (usb_host_init() != 0) {
    n = libusb_get_device_list(ctx, &devs);
    for (i = 0; i < n; i++) {
      if (libusb_get_device_descriptor(devs[i], &ddesc) != 0) {
      }
      for ((s) = ((&hostdevs)->tqh_first); (s); (s) = ((s)->tqe_next)) {
        f = &s->match;
        if (f->bus_num > 0 && f->bus_num != libusb_get_bus_number(devs[i])) {
        }
        if (usb_host_open(s, devs[i], 0) < 0) {
        }
      }
    }
    libusb_free_device_list(devs, 1);
    qemu_add_vm_change_state_handler(usb_host_vm_state, NULL);
    timer_new_ms(usb_host_auto_check, NULL);
    trace_usb_host_auto_scan_enabled();
  }
  timer_mod(usb_auto_timer, qemu_clock_get_ms() + 2000);
}
