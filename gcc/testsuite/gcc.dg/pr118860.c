/* PR middle-end/118860 */
/* { dg-do compile } */
/* { dg-options "--param=file-cache-files=180 -Wunused" } */

static void foo ();	/* { dg-warning "'foo' declared 'static' but never defined" } */
