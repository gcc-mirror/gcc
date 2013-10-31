/* { dg-do compile { target arm_thumb2 } } */
/* { dg-options "-O2 -fdump-rtl-subreg2" } */

/* { dg-final { scan-rtl-dump "REG_INC" "subreg2" } } */
/* { dg-final { cleanup-rtl-dump "subreg2" } } */
struct device;
typedef unsigned int __u32;
typedef unsigned long long u64;
typedef __u32 __le32;
typedef u64 dma_addr_t;
typedef unsigned gfp_t;
int dev_warn (const struct device *dev, const char *fmt, ...);
struct usb_bus
{
    struct device *controller;
};
struct usb_hcd
{
    struct usb_bus self;
};
struct xhci_generic_trb
{
    __le32 field[4];
};
union xhci_trb
{
    struct xhci_generic_trb generic;
};
struct xhci_segment
{
    union xhci_trb *trbs;
    dma_addr_t dma;
};
struct xhci_ring
{
    struct xhci_segment *first_seg;
};
struct xhci_hcd
{
    struct xhci_ring *cmd_ring;
    struct xhci_ring *event_ring;
};
struct usb_hcd *xhci_to_hcd (struct xhci_hcd *xhci)
{
}
dma_addr_t xhci_trb_virt_to_dma (struct xhci_segment * seg,
				 union xhci_trb * trb);
struct xhci_segment *trb_in_td (struct xhci_segment *start_seg,
				dma_addr_t suspect_dma);
xhci_test_trb_in_td (struct xhci_hcd *xhci, struct xhci_segment *input_seg,
		     union xhci_trb *start_trb, union xhci_trb *end_trb,
		     dma_addr_t input_dma, struct xhci_segment *result_seg,
		     char *test_name, int test_number)
{
    unsigned long long start_dma;
    unsigned long long end_dma;
    struct xhci_segment *seg;
    start_dma = xhci_trb_virt_to_dma (input_seg, start_trb);
    end_dma = xhci_trb_virt_to_dma (input_seg, end_trb);
    {
        dev_warn (xhci_to_hcd (xhci)->self.controller,
                  "%d\n", test_number);
        dev_warn (xhci_to_hcd (xhci)->self.controller,
                  "Expected seg %p, got seg %p\n", result_seg, seg);
    }
}
xhci_check_trb_in_td_math (struct xhci_hcd *xhci, gfp_t mem_flags)
{
    struct
    {
        dma_addr_t input_dma;
        struct xhci_segment *result_seg;
    }
    simple_test_vector[] =
        {
            {
                0, ((void *) 0)
            }
            ,
            {
                xhci->event_ring->first_seg->dma - 16, ((void *) 0)}
            ,
            {
                xhci->event_ring->first_seg->dma - 1, ((void *) 0)}
            ,
            {
                xhci->event_ring->first_seg->dma, xhci->event_ring->first_seg}
            ,
            {
                xhci->event_ring->first_seg->dma + (64 - 1) * 16,
                xhci->event_ring->first_seg
            }
            ,
            {
                xhci->event_ring->first_seg->dma + (64 - 1) * 16 + 1, ((void *) 0)}
            ,
            {
                xhci->event_ring->first_seg->dma + (64) * 16, ((void *) 0)}
            ,
            {
                (dma_addr_t) (~0), ((void *) 0)
            }
        };
    struct
    {
        struct xhci_segment *input_seg;
        union xhci_trb *start_trb;
        union xhci_trb *end_trb;
        dma_addr_t input_dma;
        struct xhci_segment *result_seg;
    }
    complex_test_vector[] =
        {
            {
                .input_seg = xhci->event_ring->first_seg,.start_trb =
                xhci->event_ring->first_seg->trbs,.end_trb =
                &xhci->event_ring->first_seg->trbs[64 - 1],.input_dma =
                xhci->cmd_ring->first_seg->dma,.result_seg = ((void *) 0),
            }
            ,
            {
                .input_seg = xhci->event_ring->first_seg,.start_trb =
                xhci->event_ring->first_seg->trbs,.end_trb =
                &xhci->cmd_ring->first_seg->trbs[64 - 1],.input_dma =
                xhci->cmd_ring->first_seg->dma,.result_seg = ((void *) 0),
            }
            ,
            {
                .input_seg = xhci->event_ring->first_seg,.start_trb =
                xhci->cmd_ring->first_seg->trbs,.end_trb =
                &xhci->cmd_ring->first_seg->trbs[64 - 1],.input_dma =
                xhci->cmd_ring->first_seg->dma,.result_seg = ((void *) 0),
            }
            ,
            {
                .input_seg = xhci->event_ring->first_seg,.start_trb =
                &xhci->event_ring->first_seg->trbs[0],.end_trb =
                &xhci->event_ring->first_seg->trbs[3],.input_dma =
                xhci->event_ring->first_seg->dma + 4 * 16,.result_seg = ((void *) 0),
            }
            ,
            {
                .input_seg = xhci->event_ring->first_seg,.start_trb =
                &xhci->event_ring->first_seg->trbs[3],.end_trb =
                &xhci->event_ring->first_seg->trbs[6],.input_dma =
                xhci->event_ring->first_seg->dma + 2 * 16,.result_seg = ((void *) 0),
            }
            ,
            {
                .input_seg = xhci->event_ring->first_seg,.start_trb =
                &xhci->event_ring->first_seg->trbs[64 - 3],.end_trb =
                &xhci->event_ring->first_seg->trbs[1],.input_dma =
                xhci->event_ring->first_seg->dma + 2 * 16,.result_seg = ((void *) 0),
            }
            ,
            {
                .input_seg = xhci->event_ring->first_seg,.start_trb =
                &xhci->event_ring->first_seg->trbs[64 - 3],.end_trb =
                &xhci->event_ring->first_seg->trbs[1],.input_dma =
                xhci->event_ring->first_seg->dma + (64 - 4) * 16,.result_seg =
                ((void *) 0),
            }
            ,
            {
                .input_seg = xhci->event_ring->first_seg,.start_trb =
                &xhci->event_ring->first_seg->trbs[64 - 3],.end_trb =
                &xhci->event_ring->first_seg->trbs[1],.input_dma =
                xhci->cmd_ring->first_seg->dma + 2 * 16,.result_seg = ((void *) 0),
            }
        };
    unsigned int num_tests;
    int i, ret;
    num_tests =
        (sizeof (simple_test_vector) / sizeof ((simple_test_vector)[0]) +
         (sizeof (struct
             {
         }
             )));
    for (i = 0; i < num_tests; i++)
    {
        ret =
            xhci_test_trb_in_td (xhci, xhci->event_ring->first_seg,
                                 xhci->event_ring->first_seg->trbs,
                                 &xhci->event_ring->first_seg->trbs[64 - 1],
                                 simple_test_vector[i].input_dma,
                                 simple_test_vector[i].result_seg, "Simple", i);
        if (ret < 0)
            return ret;
    }
    for (i = 0; i < num_tests; i++)
    {
        ret =
            xhci_test_trb_in_td (xhci, complex_test_vector[i].input_seg,
                                 complex_test_vector[i].start_trb,
                                 complex_test_vector[i].end_trb,
                                 complex_test_vector[i].input_dma,
                                 complex_test_vector[i].result_seg, "Complex", i);
        if (ret < 0)
            return ret;
    }
}
