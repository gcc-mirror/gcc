/* This combination of options tries to enable CO-RE without BTF, and should
   produce an error.  */
/* { dg-do compile } */
/* { dg-options "-gbtf -gtoggle -mco-re" } */
/* { dg-excess-errors "BPF CO-RE requires BTF debugging information" } */
