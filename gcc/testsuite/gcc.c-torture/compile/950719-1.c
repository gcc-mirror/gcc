/* { dg-require-stack-size "4092+4" } */
/* { dg-additional-options "-std=gnu89" } */

typedef struct
{
  int Header;
  char data[4092];
} t_node;

f (unsigned short rid, unsigned short record_length)
{
  t_node tnode;
  g (rid, tnode.data + rid * record_length);
}
