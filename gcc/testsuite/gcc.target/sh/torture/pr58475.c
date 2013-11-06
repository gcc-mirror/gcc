/* { dg-do compile }  */

int
kerninfo(int __bsx, double tscale)
{
 return (
	 (int)(__extension__
	       ({
		 ((((__bsx) & 0xff000000u) >> 24)
		  | (((__bsx) & 0x00ff0000) >> 8)
		  | (((__bsx) & 0x0000ff00) << 8)
		  | (((__bsx) & 0x000000ff) << 24)
		  ); }))
	       * tscale);
}
