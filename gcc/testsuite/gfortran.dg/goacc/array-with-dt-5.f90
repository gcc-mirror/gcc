type t2
  integer :: bar
end type t2
type t
  type(t2), pointer :: foo
end type t

type(t) :: c

!$acc enter data copyin(c%foo)

end
