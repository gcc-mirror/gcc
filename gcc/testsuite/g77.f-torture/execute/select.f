C   integer byte case with integer byte parameters as case(s)
        subroutine ib
        integer *1 a /1/
        integer *1  one,two,three
        parameter (one=1,two=2,three=3)
        select case (a)
        case (one)
        case (two)
           call abort
        case (three)
           call abort
        case default
           call abort
        end select
        print*,'normal ib'
        end
C   integer halfword case with integer halfword parameters
        subroutine ih
        integer *2 a /1/
        integer *2  one,two,three
        parameter (one=1,two=2,three=3)
        select case (a)
        case (one)
        case (two)
           call abort
        case (three)
           call abort
        case default
           call abort
        end select
        print*,'normal ih'
        end
C   integer case with integer parameters
        subroutine iw
        integer *4 a /1/
        integer *4  one,two,three
        parameter (one=1,two=2,three=3)
        select case (a)
        case (one)
        case (two)
           call abort
        case (three)
           call abort
        case default
           call abort
        end select
        print*,'normal iw'
        end
C   integer double case with integer double parameters
        subroutine id
        integer *8 a /1/
        integer *8  one,two,three
        parameter (one=1,two=2,three=3)
        select case (a)
        case (one)
        case (two)
           call abort
        case (three)
           call abort
        case default
           call abort
        end select
        print*,'normal id'
        end
C   integer byte select with integer case
       subroutine ib_mixed
       integer*1 s /1/
       select case (s)
       case (1)
       case (2)
         call abort
       end select
       print*,'ib ok'
       end
C   integer halfword with integer case
       subroutine ih_mixed
       integer*2 s /1/
       select case (s)
       case (1)
       case default
         call abort
       end select
       print*,'ih ok'
       end
C   integer word with integer case
       subroutine iw_mixed
       integer s /5/
       select case (s)
       case (1)
          call abort
       case (2)
          call abort
       case (3)
          call abort
       case (4)
          call abort
       case (5)
C                   
       case (6)
           call abort
       case default
           call abort
       end select
       print*,'iw ok'
       end
C   integer doubleword with integer case
       subroutine id_mixed
       integer *8 s /1024/
       select case (s)
       case (1)
           call abort
       case (1023)
           call abort
       case (1025)
           call abort
       case (1024)
C
       end select
       print*,'i8 ok'
       end
       subroutine l1_mixed
       logical*1 s /.TRUE./
       select case (s)
       case (.TRUE.)
       case (.FALSE.)
          call abort
       end select
       print*,'l1 ok'
       end
       subroutine l2_mixed
       logical*2 s /.FALSE./
       select case (s)
       case (.TRUE.)
           call abort
       case (.FALSE.)
       end select
       print*,'lh ok'
       end
       subroutine l4_mixed
       logical*4 s /.TRUE./
       select case (s)
       case (.FALSE.)
         call abort
       case (.TRUE.)
       end select
       print*,'lw ok'
       end
       subroutine l8_mixed
       logical*8 s /.TRUE./
       select case (s)
       case (.TRUE.)
       case (.FALSE.)
          call abort
       end select
       print*,'ld ok'
       end
C   main
C -- regression cases
        call ib
        call ih
        call iw
        call id
C -- new functionality
        call ib_mixed
        call ih_mixed
        call iw_mixed
        call id_mixed
        end
        




