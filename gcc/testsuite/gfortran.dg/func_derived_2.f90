! { dg-do run }
! This tests the "virtual fix" for PR19561, where functions returning
! pointers to derived types were not generating correct code.  This
! testcase is based on a simplified example in the PR discussion.
!
! Submitted by Paul Thomas  pault@gcc.gnu.org
! Slightly extended by Tobias Schlüter
module mpoint
  type           ::       mytype
    integer      ::       i
  end type mytype

contains

  function get (a) result (b)
    type (mytype), target   ::      a
    type (mytype), pointer  ::      b
    b => a
  end function get

  function get2 (a)
    type (mytype), target   ::      a
    type (mytype), pointer  ::      get2
    get2 => a
  end function get2

end module mpoint

program func_derived_2
  use mpoint
  type (mytype), target  ::       x
  type (mytype), pointer ::       y
  x = mytype (42)
  y => get (x)
  if (y%i.ne.42) call abort ()

  x = mytype (112)
  y => get2 (x)
  if (y%i.ne.112) call abort ()
end program func_derived_2
