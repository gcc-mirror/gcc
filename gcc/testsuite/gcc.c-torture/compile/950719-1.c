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
