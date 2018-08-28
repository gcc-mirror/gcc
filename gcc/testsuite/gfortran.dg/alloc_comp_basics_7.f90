! { dg-do compile }
!
! PR 86888: [F08] allocatable components of indirectly recursive type
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

type :: s
   type(t), allocatable :: x
end type

type :: t
   type(s), allocatable :: y
end type

end
