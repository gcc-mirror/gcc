! { dg-do run }
!
! Test the fix for PR84115.
!
! Contributed by G Steinmetz  <gscfq@t-online.de>
!
  character(:), allocatable :: chr
  allocate (chr, source = "abc")
  call s(chr, "abc")
  chr = "mary had a little lamb"
  call s(chr, "mary had a little lamb")
  deallocate (chr)
contains
  subroutine s(x, carg)
    character(:), allocatable :: x
    character(*) :: carg
    associate (y => x)
      if (y .ne. carg) STOP 1
    end associate
  end
end
