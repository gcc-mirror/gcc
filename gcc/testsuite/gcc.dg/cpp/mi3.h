/* Another test case for over-eager multiple include optimization. */

#ifndef GUARD
#define GUARD
#elif 1				/* #elif kills optimisation  */
int c;
#endif
