// Build don't link: 
// Special g++ Options: -fexceptions
// GROUPS passed exceptions
// except file
// From: GUSTAVO%DRAGON@orion.cpqd.ansp.br
// Date:     15 Dec 1993 09:33:30 +0000 (C)
// Subject:  exception handling problem
// Message-ID: <01H6I5GEAF5UPBJ0UV@VENUS.CPQD.ANSP.BR>

struct Exception
 {
     int v;
     Exception(int i) { v = i; };
 };

void inc(int &i)
 {
     try {
         if (i == 0)
             throw Exception(i);
         else
             i++;
     }
     catch (Exception v) {
         i = v.v;
     }
 }
