/* -g implies BTF, -gtoggle turns it off.  CO-RE should not work.  */
/* { dg-do compile } */
/* { dg-options "-g -mco-re -gtoggle" } */
/* { dg-excess-errors "BPF CO-RE requires BTF debugging information" } */
