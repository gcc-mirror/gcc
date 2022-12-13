/* We used to ICE in the gimplifier, PR 106764 */
/* { dg-do compile } */
/* { dg-options "-w" } */
(*a)(); // { dg-note "" }
b(){a()} a; // { dg-error "" }
