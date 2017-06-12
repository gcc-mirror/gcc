! { dg-do compile }
!
! PR 80046: [F03] Explicit interface required: pointer argument
!
! Contributed by Joachim Herb <joachim.herb@gmx.de>

program p
  implicit none

  type :: Node_t
    procedure(NodeCloner), nopass, pointer :: cloneProc => NULL()
    procedure(), nopass, pointer :: noIfc => NULL()
  end type

  interface
    subroutine NodeCloner( tgt, src )
      import Node_t
      type(Node_t), pointer, intent(out) :: tgt
      type(Node_t),           intent(in) :: src
    end subroutine
  end interface

  type(Node_t) :: node
  procedure(NodeCloner), pointer :: cloneNode
  procedure(), pointer :: noIfc

  cloneNode => node%noIfc       ! { dg-error "Explicit interface required" }
  node%noIfc => cloneNode       ! { dg-error "Explicit interface required" }

  noIfc => node%cloneProc       ! { dg-error "Explicit interface required" }
  node%cloneProc => noIfc       ! { dg-error "Explicit interface required" }

  node%cloneProc => node%noIfc  ! { dg-error "Explicit interface required" }
  node%noIfc => node%cloneProc  ! { dg-error "Explicit interface required" }

  ! the following cases are legal

  node%noIfc => node%noIfc
  node%cloneProc => node%cloneProc

  cloneNode => node%cloneProc
  node%cloneProc => cloneNode

  noIfc => node%noIfc
  node%noIfc => noIfc

end
