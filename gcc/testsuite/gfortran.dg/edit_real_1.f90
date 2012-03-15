! { dg-do run }
! Check real value edit descriptors
! Also checks that rounding is performed correctly
program edit_real_1
  character(len=20) s
  character(len=20) x
  character(len=200) t
  parameter (x = "xxxxxxxxxxxxxxxxxxxx")

  ! W append a "z" onto each test to check the field is the correct width
  s = x
  ! G -> F format
  write (s, '(G10.3,A)') 12.36, "z"
  if (s .ne. "  12.4    z") call abort
  s = x
  ! G -> E format
  write (s, '(G10.3,A)') -0.0012346, "z"
  if (s .ne. "-0.123E-02z") call abort
  s = x
  ! Gw.eEe format
  write (s, '(G10.3e1,a)') 12.34, "z"
  if (s .ne. "   12.3   z") call abort
  ! E format with excessive precision
  write (t, '(E199.192,A)') 1.5, "z"
  if ((t(1:7) .ne. " 0.1500") .or. (t(194:200) .ne. "00E+01z")) call abort
  ! EN format
  s = x
  write (s, '(EN15.3,A)') 12873.6, "z"
  if (s .ne. "     12.874E+03z") call abort
  ! EN format, negative exponent
  s = x
  write (s, '(EN15.3,A)') 12.345e-6, "z"
  if (s .ne. "     12.345E-06z") call abort
  ! ES format
  s = x
  write (s, '(ES10.3,A)') 16.235, "z"
  if (s .ne. " 1.624E+01z") call abort
  ! F format, small number
  s = x
  write (s, '(F10.8,A)') 1.0e-20, "z"
  if (s .ne. "0.00000000z") call abort
  ! E format, very large number.
  ! Used to overflow with positive scale factor
  s = x
  write (s, '(1PE10.3,A)') huge(0d0), "z"
  ! The actual value is target specific, so just do a basic check
  if ((s(1:1) .eq. "*") .or. (s(7:7) .ne. "+") .or. &
      (s(11:11) .ne. "z")) call abort
  ! F format, round up with carry to most significant digit.
  s = x
  write (s, '(F10.3,A)') 0.9999, "z"
  if (s .ne. "     1.000z") call abort
  ! F format, round up with carry to most significant digit < 0.1.
  s = x
  write (s, '(F10.3,A)') 0.0099, "z"
  if (s .ne. "     0.010z") call abort
  ! E format, round up with carry to most significant digit.
  s = x
  write (s, '(E10.3,A)') 0.9999, "z"
  if (s .ne. " 0.100E+01z") call abort
  ! EN format, round up with carry to most significant digit.
  s = x
  write (s, '(EN15.3,A)') 999.9999, "z"
  if (s .ne. "      1.000E+03z") call abort
  ! E format, positive scale factor
  s = x
  write (s, '(2PE10.4,A)') 1.2345, "z"
  if (s .ne. '12.345E-01z') call abort
  ! E format, negative scale factor
  s = x
  write (s, '(-2PE10.4,A)') 1.250001, "z"
  if (s .ne. '0.0013E+03z') call abort
  ! E format, single digit precision
  s = x
  write (s, '(E10.1,A)') 1.1, "z"
  if (s .ne. '   0.1E+01z') call abort
end

