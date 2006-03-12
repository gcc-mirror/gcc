/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-dom3" } */
     

extern const unsigned char mode_size[];
unsigned int
subreg_highpart_offset (outermode, innermode)
     int outermode, innermode;
{
  unsigned int offset = 0;
  int difference = (mode_size[innermode] - mode_size[outermode]);
  if (difference > 0)
    {
        offset += difference % (0 ? 8 : 4);
	offset += difference / 4 * 4;
    }
  return offset;
}

/* There should be one mask with the value 3.  */
/* { dg-final { scan-tree-dump-times " \& 3" 1 "dom3"} } */
  
/* There should be one right shift by 2 places.  */
/* { dg-final { scan-tree-dump-times " >> 2" 1 "dom3"} } */

/* { dg-final { cleanup-tree-dump "dom3" } } */
