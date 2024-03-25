/* { dg-additional-options "-std=gnu89" } */

x(float*x){int a[4],i=0,j;for(j=0;j<2;j++){f(a[i++]);f(a[i++]);}}
