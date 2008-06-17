! { dg-do compile }
! Tests the fix for PR24813 in which a character array
! constructor, as an argument for LEN, would cause an ICE.
!
  character(11) :: chr1, chr2
  i = len ((/chr1, chr2, "ggg        "/))
  j = len ((/"abcdefghijk", chr1, chr2/))
  k = len ((/'hello  ','goodbye'/))
  l = foo ("yes siree, Bob")
  if (any ((/11,11,7,14/) /= (/i,j,k,l/))) call abort ()
contains
  integer function foo (arg)
    character(*) :: arg
    character(len(arg)) :: ctor
    foo = len ((/ctor/))
  end function foo
end
