! { dg-do compile }
! { dg-options "-fbackslash" }

  character(len=20,kind=4) :: s4
  character(len=20,kind=1) :: s1

  s1 = "foo\u0000"
  s1 = "foo\u00ff"
  s1 = "foo\u0100" ! { dg-error "is not representable" }
  s1 = "foo\u0101" ! { dg-error "is not representable" }
  s1 = "foo\U00000101" ! { dg-error "is not representable" }

  s1 = 4_"foo bar"
  s1 = 4_"foo\u00ff"
  s1 = 4_"foo\u0101" ! { dg-error "cannot be converted" }
  s1 = 4_"foo\u1101" ! { dg-error "cannot be converted" }
  s1 = 4_"foo\UFFFFFFFF" ! { dg-error "cannot be converted" }

  s4 = "foo\u0000"
  s4 = "foo\u00ff"
  s4 = "foo\u0100" ! { dg-error "is not representable" }
  s4 = "foo\U00000100" ! { dg-error "is not representable" }

  s4 = 4_"foo bar"
  s4 = 4_"\xFF\x96"
  s4 = 4_"\x00\x96"
  s4 = 4_"foo\u00ff"
  s4 = 4_"foo\u0101"
  s4 = 4_"foo\u1101"
  s4 = 4_"foo\Uab98EF56"
  s4 = 4_"foo\UFFFFFFFF"

end
