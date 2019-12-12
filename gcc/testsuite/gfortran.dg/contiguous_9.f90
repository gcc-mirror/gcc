program contiguous_pointer

type t
end type t

type s
   class(t), dimension(:), contiguous, pointer :: x ! OK
   class(t), contiguous, allocatable :: y ! { dg-error "has the CONTIGUOUS attribute but is not an array pointer" }
   class(t), contiguous, pointer :: z ! { dg-error "has the CONTIGUOUS attribute but is not an array pointer" }
end type s

end program contiguous_pointer
