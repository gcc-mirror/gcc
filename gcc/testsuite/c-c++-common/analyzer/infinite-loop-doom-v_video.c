/* Reduced from a -Wanalyzer-infinite-loop false positive seen in Doom's
   v_video.c, which is under the GPLv2 or later.  */

typedef unsigned char byte;

byte*				screens[5];

#define SCREENWIDTH 320

void
V_DrawBlock
( int		x,
  int		y,
  int		scrn,
  int		width,
  int		height,
  byte*		src ) 
{ 
    byte*	dest; 

    /* [...snip...] */
 
    dest = screens[scrn] + y*SCREENWIDTH+x; 

    while (height--) /* { dg-bogus "infinite loop" } */
    { 
	__builtin_memcpy (dest, src, width); 
	src += width; 
	dest += SCREENWIDTH; 
    } 
} 
