/* { dg-do compile { target *-*-darwin* } } */

/* Here we want to test if "foo" gets placed into a coalesced
   section (it should not).

   However, for i386, and PIC code we have a "get_pc thunk" that
   is (correctly) placed in a coalesced section when using an older
   linker - also unwind tables are emitted into coalesced.

   With modern linkers this is moot, since even weak symbols
   are emitted into the regular sections.

   To avoid the unwind tables -fno-asynchronous-unwind-tables.
   To ensure that we emit code for an older linker -mtarget-linker
   To avoid the get_pc thunk optimise at least O1.  */

/* { dg-options "-fno-asynchronous-unwind-tables -O1 -mtarget-linker 85.2" } */
/* { dg-require-weak "" } */

/* { dg-final { scan-assembler-not "coalesced" } } */

extern void foo(void) __attribute__((weak_import));

void foo(void)
{
}
