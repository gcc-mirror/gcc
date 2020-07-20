/* { dg-do compile } */
/* { dg-additional-options "-g" } */

typedef long unsigned int size_t;
typedef signed int __int32_t;
typedef __int32_t int32_t;
typedef long int ptrdiff_t;
typedef enum {
  BT_UNKNOWN = 0, BT_INTEGER, BT_LOGICAL, BT_REAL, BT_COMPLEX,   BT_DERIVED, BT_CHARACTER, BT_CLASS, BT_PROCEDURE, BT_HOLLERITH, BT_VOID,   BT_ASSUMED, BT_UNION, BT_BOZ } bt;
typedef int32_t GFC_INTEGER_4;
typedef ptrdiff_t index_type;
typedef size_t gfc_charlen_type;
typedef struct descriptor_dimension {
    index_type _stride;
    index_type lower_bound;
  } descriptor_dimension;
typedef struct {
  descriptor_dimension dim[15];
 } gfc_full_array_i4;
typedef void (*formatted_dtio)(void *, GFC_INTEGER_4 *, char *,           gfc_full_array_i4 *,           GFC_INTEGER_4 *, char *,           gfc_charlen_type, gfc_charlen_type);
typedef enum { DECIMAL_POINT, DECIMAL_COMMA, DECIMAL_UNSPECIFIED } unit_decimal;
typedef struct st_parameter_dt {
      union     {
	  struct  {
	      struct gfc_unit *current_unit;
	      unsigned namelist_mode : 1;
	      unsigned unit_is_internal : 1;
	      formatted_dtio fdtio_ptr;
	  } p;
      } u;
  } st_parameter_dt;
typedef struct gfc_unit {
    int unit_number;
    unit_decimal decimal_status;
    int (*next_char_fn_ptr) (st_parameter_dt *);
    void (*push_char_fn_ptr) (st_parameter_dt *, int);
  } gfc_unit;
void read_real (st_parameter_dt *dtp)
{
  int c;
  int seen_dp;
  seen_dp = 0;
  for (;;)
    {
      c = ((dtp)->u.p.current_unit->next_char_fn_ptr (dtp));
      if (c == ',' && dtp->u.p.current_unit->decimal_status == DECIMAL_COMMA)
	c = '.';
      switch (c)  {
	case '.':
	  if (seen_dp)      goto bad_real;
	  seen_dp = 1;
	  ((dtp)->u.p.current_unit->push_char_fn_ptr (dtp, c));
	  goto real_loop;
	case 'E':
	case 'e':
	case 'D':
	case 'd':
	case 'Q':
	case 'q':
	     goto exp1;
	case '+':
	case '-':
	     ((dtp)->u.p.current_unit->push_char_fn_ptr (dtp, 'e'));
	     goto got_repeat;
      }
    }
got_repeat:
real_loop:
  for (;;)
    {
      c = ((dtp)->u.p.current_unit->next_char_fn_ptr (dtp));
      switch (c)  {
	case '.':
	  if (seen_dp)      goto bad_real;
	  seen_dp = 1;
	  ((dtp)->u.p.current_unit->push_char_fn_ptr (dtp, c));
      }
    }
exp1:
bad_real:
  return;
}
