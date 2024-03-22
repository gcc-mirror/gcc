/* { dg-do compile }
 * { dg-options "-std=c23" }
 */

enum A { N = 0 * sizeof(enum A { M = 1 }) }; 	/* { dg-error "nested" } */


