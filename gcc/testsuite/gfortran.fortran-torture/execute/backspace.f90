! pr 15755
        implicit none
        character*1 C
        open(10)
        write(10,*)'a'
        write(10,*)'b'
        write(10,*)'c'
        rewind(10)
        read(10,*)C
        backspace(10) 
        read(10,*) C
        if (C.ne.'a') call abort
        close(10,STATUS='DELETE')
        end
