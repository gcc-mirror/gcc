/* { dg-do compile } */
/* { dg-require-effective-target fstack_protector }*/
/* { dg-require-effective-target fpic }*/
/* { dg-additional-options "-Os -fpic -fstack-protector-strong" } */

#include <stddef.h>
#include <stdint.h>


static const unsigned char base64_enc_map[64] =
{
    'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J',
    'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T',
    'U', 'V', 'W', 'X', 'Y', 'Z', 'a', 'b', 'c', 'd',
    'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n',
    'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x',
    'y', 'z', '0', '1', '2', '3', '4', '5', '6', '7',
    '8', '9', '+', '/'
};

#define BASE64_SIZE_T_MAX   ( (size_t) -1 ) /* SIZE_T_MAX is not standard */


void doSmth(void *x);

#include <string.h>


void check(int n) {
  
    if (!(n % 2 && n % 3 && n % 5)) {
 __asm__  (   "add    r8, r8, #1;" );
    }
}

uint32_t test(
  uint32_t a1,
  uint32_t a2,
  size_t a3,
  size_t a4,
  size_t a5,
  size_t a6)
{
  uint32_t nResult = 0;
  uint8_t* h = 0L;
  uint8_t X[128];
  uint8_t mac[64];
  size_t len;

  doSmth(&a1);
  doSmth(&a2);
  doSmth(&a3);
  doSmth(&a4);
  doSmth(&a5);
  doSmth(&a6);

  if (a1 && a2 && a3 && a4 && a5 && a6) {
    nResult = 1;
    h = (void*)X;
    len = sizeof(X);
    memset(X, a2, len);
    len -= 64;
    memcpy(mac ,X, len);
    *(h + len) = a6;

    {


        unsigned char *dst = X;
        size_t dlen = a3;
        size_t *olen = &a6;
        const unsigned char *src = mac;
        size_t slen = a4;
    size_t i, n;
    int C1, C2, C3;
    unsigned char *p;

    if( slen == 0 )
    {
        *olen = 0;
        return( 0 );
    }

    n = slen / 3 + ( slen % 3 != 0 );

    if( n > ( BASE64_SIZE_T_MAX - 1 ) / 4 )
    {
        *olen = BASE64_SIZE_T_MAX;
        return( 0 );
    }

    n *= 4;

    if( ( dlen < n + 1 ) || ( NULL == dst ) )
    {
        *olen = n + 1;
        return( 0 );
    }

    n = ( slen / 3 ) * 3;

    for( i = 0, p = dst; i < n; i += 3 )
    {
        C1 = *src++;
        C2 = *src++;
        C3 = *src++;

        check(i);

        *p++ = base64_enc_map[(C1 >> 2) & 0x3F];
        *p++ = base64_enc_map[(((C1 &  3) << 4) + (C2 >> 4)) & 0x3F];
        *p++ = base64_enc_map[(((C2 & 15) << 2) + (C3 >> 6)) & 0x3F];
        *p++ = base64_enc_map[C3 & 0x3F];
    }

    if( i < slen )
    {
        C1 = *src++;
        C2 = ( ( i + 1 ) < slen ) ? *src++ : 0;

        *p++ = base64_enc_map[(C1 >> 2) & 0x3F];
        *p++ = base64_enc_map[(((C1 & 3) << 4) + (C2 >> 4)) & 0x3F];

        if( ( i + 1 ) < slen )
             *p++ = base64_enc_map[((C2 & 15) << 2) & 0x3F];
        else *p++ = '=';

        *p++ = '=';
    }

    *olen = p - dst;
    *p = 0;

}

  __asm__ ("mov r8, %0;" : "=r" ( nResult ));
  }
  else
  {
    nResult = 2;
  }

  doSmth(X);
  doSmth(mac);


  return nResult;
}

/* The pattern below catches sequences of instructions that were generated
   for ARM and Thumb-2 before the fix for this PR. They are of the form:

   ldr     rX, <offset from sp or fp>
   <optional non ldr instructions>
   ldr     rY, <offset from sp or fp>
   ldr     rZ, [rX]
   <optional non ldr instructions>
   cmp     rY, rZ
   <optional non cmp instructions>
   bl      __stack_chk_fail

   Ideally the optional block would check for the various rX, rY and rZ
   registers not being set but this is not possible due to back references
   being illegal in lookahead expression in Tcl, thus preventing to use the
   only construct that allow to negate a regexp from using the backreferences
   to those registers.  Instead we go for the heuristic of allowing non ldr/cmp
   instructions with the assumptions that (i) those are not part of the stack
   protector sequences and (ii) they would only be scheduled here if they don't
   conflict with registers used by stack protector.

   Note on the regexp logic:
   Allowing non X instructions (where X is ldr or cmp) is done by looking for
   some non newline spaces, followed by something which is not X, followed by
   an alphanumeric character followed by anything but a newline and ended by a
   newline the whole thing an undetermined number of times. The alphanumeric
   character is there to force the match of the negative lookahead for X to
   only happen after all the initial spaces and thus to check the mnemonic.
   This prevents it to match one of the initial space.  */
/* { dg-final { scan-assembler-not {ldr[ \t]+([^,]+), \[(?:sp|fp)[^]]*\](?:\n[ \t]+(?!ldr)\w[^\n]*)*\n[ \t]+ldr[ \t]+([^,]+), \[(?:sp|fp)[^]]*\]\n[ \t]+ldr[ \t]+([^,]+), \[\1\](?:\n[ \t]+(?!ldr)\w[^\n]*)*\n[ \t]+cmp[ \t]+\2, \3(?:\n[ \t]+(?!cmp)\w[^\n]*)*\n[ \t]+bl[ \t]+__stack_chk_fail} } } */

/* Likewise for Thumb-1 sequences of instructions prior to the fix for this PR
   which had the form:

   ldr     rS, <offset from sp or fp>
   <optional non ldr instructions>
   ldr     rT, <PC relative offset>
   <optional non ldr instructions>
   ldr     rX, [rS, rT]
   <optional non ldr instructions>
   ldr     rY, <offset from sp or fp>
   ldr     rZ, [rX]
   <optional non ldr instructions>
   cmp     rY, rZ
   <optional non cmp instructions>
   bl      __stack_chk_fail

  Note on the regexp logic:
  PC relative offset is checked by looking for a source operand that does not
  contain [ or ].  */
/* { dg-final { scan-assembler-not {ldr[ \t]+([^,]+), \[(?:sp|fp)[^]]*\](?:\n[ \t]+(?!ldr)\w[^\n]*)*\n[ \t]+ldr[ \t]+([^,]+), [^][\n]*(?:\n[ \t]+(?!ldr)\w[^\n]*)*\n[ \t]+ldr[ \t]+([^,]+), \[\1, \2\](?:\n[ \t]+(?!ldr)\w[^\n]*)*\n[ \t]+ldr[ \t]+([^,]+), \[(?:sp|fp)[^]]*\]\n[ \t]+ldr[ \t]+([^,]+), \[\3\](?:\n[ \t]+(?!ldr)\w[^\n]*)*\n[ \t]+cmp[ \t]+\4, \5(?:\n[ \t]+(?!cmp)\w[^\n]*)*\n[ \t]+bl[ \t]+__stack_chk_fail} } } */
