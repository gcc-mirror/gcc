typedef enum
{
  no_op,
  jump
}
re_opcode_t;
struct
{
}
byte_register_info_type ()
{
  char *p;
  for (;;)
    switch ((re_opcode_t) p++)
      {
      case no_op:
	{
	  for (; (p);)
	    ;
	    for (;;)
	    ;
	}
      case jump:
	(p) += 2;
      }
}
