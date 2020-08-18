/* Integration test to verify that we don't explode in this
   argument-parsing logic.
   Adapted from part of bzip2-1.0.8: bzip2.c: main.  */

#include <stdlib.h>
#include <stdio.h>
#include "analyzer-decls.h"

/* This test file has been heavily modified from the bzip2.c original,
   which has the following license boilerplate.  */
/* ------------------------------------------------------------------
   This file is part of bzip2/libbzip2, a program and library for
   lossless, block-sorting data compression.

   bzip2/libbzip2 version 1.0.8 of 13 July 2019
   Copyright (C) 1996-2019 Julian Seward <jseward@acm.org>

   Please read the WARNING, DISCLAIMER and PATENTS sections in the 
   README file.

   This program is released under the terms of the license contained
   in the file LICENSE.
   ------------------------------------------------------------------ */

typedef char            Char;
typedef unsigned char   Bool;
typedef int             Int32;

#define True  ((Bool)1)
#define False ((Bool)0)

typedef
   struct zzzz {
      Char        *name;
      struct zzzz *link;
   }
   Cell;

Int32   verbosity;
Bool    keepInputFiles, smallMode;
Bool    forceOverwrite, noisy;
Int32   blockSize100k;
Int32   opMode;
Int32   srcMode;
Char    *progName;

extern void license ( void );
extern void usage ( Char *fullProgName );

void test (Cell   *argList)
{
   Cell   *aa;
   Int32  i, j;

   for (aa = argList; aa != NULL; aa = aa->link) {
      if (aa->name[0] == '-' && aa->name[1] != '-') {
         for (j = 1; aa->name[j] != '\0'; j++) {
            switch (aa->name[j]) {
               case 'c': srcMode          = 2; break;
               case 'd': opMode           = 2; break;
               case 'z': opMode           = 1; break;
               case 'f': forceOverwrite   = True; break;
               case 't': opMode           = 3; break;
               case 'k': keepInputFiles   = True; break;
               case 's': smallMode        = True; break;
               case 'q': noisy            = False; break;
               case '1': blockSize100k    = 1; break;
               case '2': blockSize100k    = 2; break;
               case '3': blockSize100k    = 3; break;
               case '4': blockSize100k    = 4; break;
               case '5': blockSize100k    = 5; break;
               case '6': blockSize100k    = 6; break;
               case '7': blockSize100k    = 7; break;
               case '8': blockSize100k    = 8; break;
               case '9': blockSize100k    = 9; break;
               case 'V':
               case 'L': license();            break;
               case 'v': verbosity++; break;
               case 'h': usage ( progName );
                         exit ( 0 );
                         break;
               default:  fprintf ( stderr, "%s: Bad flag `%s'\n",
                                   progName, aa->name );
                         usage ( progName );
                         exit ( 1 );
                         break;
            }
         }
      }
   }

   /* The analyzer ought to be able to successfully merge all of the
      above changes that can reach here into a single state.  */
   __analyzer_dump_exploded_nodes (0); /* { dg-warning "1 processed enode" } */
}
