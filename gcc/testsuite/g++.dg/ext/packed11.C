// PR c++/26670

struct nonpod {
  nonpod();
};

struct nonpod_pack {
  nonpod n;		      // { dg-warning "ignoring packed attribute" }
} __attribute__ ((packed));

struct nonpod_pack2 {
  nonpod_pack p;	      // { dg-warning "ignoring packed attribute" }
} __attribute__ ((packed));
