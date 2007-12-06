! { dg-do compile }
! Tests the fix for PR34335 a regression in which the PRIVATE attribute
! of type(a) in module b would be ignored and would prevent it being
! loaded in the main program.
!
! Contributed by Janus Weil <jaydub66@gmail.com>
!
module A
  type A_type
    real comp
  end type
end module A

module B
  use A
  private
  type(A_type) :: B_var
  public:: B_var
end module B

program C
  use B
  use A
  type(A_type):: A_var
end program C
! { dg-final { cleanup-modules "a b" } }
