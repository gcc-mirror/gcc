/* { dg-do preprocess } */

#define nop()		foobar
	 nop
	call b

/*
   { dg-final { if ![file exists 20000720-1.i] { return }         } }
   { dg-final { if \{ [grep 20000720-1.i "nop.*call"] != "" \} \{ } }
   { dg-final {     fail "20000720-1.S: new-line preservation"    } }
   { dg-final { \} else \{                                        } }
   { dg-final {     pass "20000720-1.S: new-line preservation"    } }
   { dg-final { \}                                                } }
*/
