/* PR rtl-optimization/20527
   Mishandled postincrement.  This test-case is derived from the
   function BZ2_hbCreateDecodeTables in the file huffman.c from
   bzip2-1.0.2, hence requiring the following disclaimer copied here:  */

/*--
  This file is a part of bzip2 and/or libbzip2, a program and
  library for lossless, block-sorting data compression.

  Copyright (C) 1996-2002 Julian R Seward.  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions
  are met:

  1. Redistributions of source code must retain the above copyright
     notice, this list of conditions and the following disclaimer.

  2. The origin of this software must not be misrepresented; you must 
     not claim that you wrote the original software.  If you use this 
     software in a product, an acknowledgment in the product 
     documentation would be appreciated but is not required.

  3. Altered source versions must be plainly marked as such, and must
     not be misrepresented as being the original software.

  4. The name of the author may not be used to endorse or promote 
     products derived from this software without specific prior written 
     permission.

  THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS
  OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
  DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
  GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
  WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

  Julian Seward, Cambridge, UK.
  jseward@acm.org
  bzip2/libbzip2 version 1.0 of 21 March 2000

  This program is based on (at least) the work of:
     Mike Burrows
     David Wheeler
     Peter Fenwick
     Alistair Moffat
     Radford Neal
     Ian H. Witten
     Robert Sedgewick
     Jon L. Bentley

  For more information on these sources, see the manual.
--*/

void f (long *limit, long *base, long minLen, long maxLen) __attribute__ ((__noinline__));
void f (long *limit, long *base, long minLen, long maxLen)
{
  long i;
  long vec;
  vec = 0;
  for (i = minLen; i <= maxLen; i++) {
    vec += (base[i+1] - base[i]);
    limit[i] = vec-1;
  }
}
extern void abort (void);
extern void exit (int);
long b[] = {1, 5, 11, 23};
int main (void)
{
  long l[3];
  f (l, b, 0, 2);
  if (l[0] != 3 || l[1] != 9 || l[2] != 21)
    abort ();
  exit (0);
}
