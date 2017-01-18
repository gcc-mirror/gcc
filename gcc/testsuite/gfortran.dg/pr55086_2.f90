! { dg-do run }
!
  implicit none

  character(len=7), pointer :: u
  character(len=7), pointer :: v

  character(len=7), target  :: a
  character(len=7), target  :: b

  integer :: j

  b = "1234567"
  a = "abcdefg"

  u => a
  v => b

  forall (j = 1:2) a(j:j) = b(j:j)

  if (a /= "12cdefg") call abort

  forall (j = 2:3) a(j:j) = v(j:j)
  if (a /= "123defg") call abort

  forall (j = 3:4) u(j:j) = b(j:j)
  if (a /= "1234efg") call abort

  forall (j = 4:5) u(j:j) = v(j:j)
  if (a /= "12345fg") call abort

end
