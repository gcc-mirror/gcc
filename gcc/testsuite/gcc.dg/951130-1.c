/* { dg-do compile }
   { dg-options "-Werror" } */

unsigned long long x = -(unsigned long long)(-(long long)
				(((unsigned long long)0 - 1) >> 1) - 1);
