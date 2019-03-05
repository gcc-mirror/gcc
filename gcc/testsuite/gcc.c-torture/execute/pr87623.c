/* PR middle-end/87623 */
/* Testcase by George Thopas <george.thopas@gmail.com> */

struct be {
    unsigned short pad[1];
    unsigned char  a;
    unsigned char  b;
} __attribute__((scalar_storage_order("big-endian")));

typedef struct be t_be;

struct le {
    unsigned short pad[3];
    unsigned char  a;
    unsigned char  b;
};

typedef struct le t_le;

int a_or_b_different(t_be *x,t_le *y)
{
   return (x->a != y->a) || (x->b != y->b);
}

int main (void)
{
   t_be x = { .a=1, .b=2  };
   t_le y = { .a=1, .b=2  };
  
   if (a_or_b_different(&x,&y))
       __builtin_abort ();

   return 0;
}
