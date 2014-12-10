/* { dg-require-effective-target indirect_jumps } */

int jump () { goto * (int (*) ()) 0xbabebec0; }

