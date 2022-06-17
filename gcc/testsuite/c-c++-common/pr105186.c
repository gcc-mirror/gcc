/* PR c++/105186 */
/* { dg-do compile } */

__attribute__((__int128)) int i;	/* { dg-warning "'__int128' attribute directive ignored" } */
__attribute__((__int128__)) int j;	/* { dg-warning "'__int128' attribute directive ignored" } */
