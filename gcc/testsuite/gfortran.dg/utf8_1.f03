! { dg-do run }
! { dg-options "-fbackslash" }
! Contributed by Jerry DeLisle  <jvdelisle@gcc.gnu.org>
program test1
  implicit none
  integer, parameter :: k4 = 4
  character(kind=4, len=30) :: string1, string2
  character(kind=1, len=30) :: string3
  string1 = k4_"This is Greek:  \u039f\u03cd\u03c7\u03af"
  string2 = k4_"Jerry in Japanese is:    \u30b8\u30a8\u30ea\u30fc"
  open(10, encoding="utf-8", status="scratch")
  write(10,'(a)') trim(string1)
  write(10,*) string2
  rewind(10)
  string1 = k4_""
  string2 = k4_""
  string3 = "abcdefghijklmnopqrstuvwxyz"
  read(10,'(a)') string1
  read(10,'(a)') string2
  if (string1 /= k4_"This is Greek:  \u039f\u03cd\u03c7\u03af") STOP 1
  if (len(trim(string1)) /= 20) STOP 2
  if (string2 /= k4_" Jerry in Japanese is:    \u30b8\u30a8\u30ea\u30fc")&
  &  STOP 3
  if (len(string2) /= 30) STOP 4
  rewind(10)
  read(10,'(a)') string3
  if (string3 /= "This is Greek:  ????") STOP 5
end program test1
! The following examples require UTF-8 enabled editor to see correctly.
! ジエリー  Sample of Japanese characters.
! Οὐχὶ    Sample of Greek characters.
