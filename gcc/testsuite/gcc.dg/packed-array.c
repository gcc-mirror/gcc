/* { dg-do run } */
/* { dg-options "-O2 -fno-inline" } */

struct usb_interface_descriptor {
 unsigned short wMaxPacketSize;
  char e;
} __attribute__ ((packed));

struct usb_device {
 int devnum;
 struct usb_interface_descriptor if_desc[2];
};

extern int printf (const char *, ...);

void foo (unsigned short a)
{
  printf ("%d\n", a);
}

struct usb_device ndev;

void usb_set_maxpacket(int n)
{
  int i;

  for(i=0; i<n;i++)
    foo((&ndev)->if_desc[i].wMaxPacketSize);
}

int
main()
{
  usb_set_maxpacket(2);
  return 0;
}




