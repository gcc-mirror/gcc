/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-dom3" } */

extern void abort (void);
typedef unsigned int size_t;
struct rtx_def;
typedef struct rtx_def *rtx;
typedef union varray_data_tag
{
  struct reg_info_def *reg[1];
} varray_data;
struct varray_head_tag
{
  size_t num_elements;
  varray_data data;
};
typedef struct varray_head_tag *varray_type;
typedef struct reg_info_def
{
} reg_info;
extern varray_type reg_n_info;
static rtx *reg_base_value;
static rtx *new_reg_base_value;

rtx
blah (unsigned int regno)
{
  if (new_reg_base_value[regno] && ((*(
					{
					if (regno >=
					    reg_n_info->
					    num_elements)
					abort ();
					&reg_n_info->data.reg[regno];}
				     ))))
    return reg_base_value[regno];
}

/* If we have more than 1 cast to a struct rtx_def * *, then we failed to
   eliminate some useless typecasting.  The first type cast is needed
   to convert the unsigned int regno parameter into a struct rtx_def **.  */
/* { dg-final { scan-tree-dump-times "\\(struct rtx_def \\* \\*\\)" 1 "dom3"} } */
/* { dg-final { cleanup-tree-dump "dom3" } } */
