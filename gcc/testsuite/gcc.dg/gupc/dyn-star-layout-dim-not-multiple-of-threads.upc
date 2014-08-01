/* A UPC layout qualifier of '[*]' requires that
   the array size is either an integral constant
   or an integral multiple of THREADS.
	{ dg-do compile }
	{ dg-options "-fupc-threads=0" } */

extern int THREADS;
/* The base address of the UPC shared section */
extern char __upc_shared_start[1];



shared [*] int A[THREADS+1]; /* { dg-error "UPC shared array dimension is not a simple multiple of THREADS; the size of 'A' cannot be calculated." } */
/* { dg-prune-output "a UPC layout qualifier of '\\\[\\*\\\]' requires that the array size is either an integral constant or an integral multiple of THREADS" } */
