/* PR target/11805.  */

/* Consider the following sequence.

     (set (cc0)
	  (and:HI (reg:HI 0)
		  (const_int 1)))

     (set (pc)
	  (if_then_else (le (cc0)
			    (const_int 0))
			(label_ref 17)
			(pc)))

   On h8300, the first insn does not set the overflow flag, but the
   second requires the overflow flag.  As a result, when the final
   wants to output the jump insn, it cannot find a test insn that
   gives appropriate condition flags.  */

unsigned char
foo (unsigned char a)
{
  return (a & 1) > 0;
}
