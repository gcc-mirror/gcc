*> { dg-do run }
*> { dg-output {we saw 09 records; there should have been 09} }
        identification division.
        program-id.  line-seq.
        environment division.
        input-output section.
        file-control.
            select  data-file
            assign to
            "data.tab" organization line sequential.
        data division.
        file section.
        fd  data-file.
        01  data-record                  pic x(80).
        working-storage section.
        01 record-count pic 99 value zero.
        procedure division.
        move "I am a line" to data-record
        open    output data-file.
        perform 9 times
            write data-record
            end-perform
        close data-file
        open    input   data-file.
        read-loop.
        read    data-file
                at end
                display "we saw " record-count " records; there should"
                        " have been 09"
                close data-file
                stop run.
        add 1 to record-count
        go to read-loop.
        end program line-seq.
