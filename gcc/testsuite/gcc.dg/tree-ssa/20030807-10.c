/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-vrp1" } */
     

extern const unsigned char mode_size[];
unsigned int
subreg_highpart_offset (outermode, innermode)
     int outermode, innermode;
{
  unsigned int offset = 1;
  int difference = (mode_size[innermode] - mode_size[outermode]);
  if (difference > 0)
    {
        offset += difference % (0 ? 8 : 4);
	offset += difference / 4 * 4;
    }
  return offset;
}

/* There should be one mask with the value 3.  */
/* { dg-final { scan-tree-dump-times " \& 3" 1 "vrp1"} } */
  
/* There should be one right shift by 2 places.  */
/* { dg-final { scan-tree-dump-times " >> 2" 1 "vrp1"} } */

