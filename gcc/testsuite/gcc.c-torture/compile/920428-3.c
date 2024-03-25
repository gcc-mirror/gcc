/* { dg-require-effective-target label_values } */
/* { dg-require-effective-target indirect_jumps } */

int x(int a){static void*j[]={&&l1,&&l2};goto*j[a];l1:return 0;l2:return 1;}
