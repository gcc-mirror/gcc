       *> { dg-do run }
       *> { dg-output-file "group2/STRING___UNSTRING__NOT__ON_OVERFLOW.out" }

       identification division.
       program-id. prog.
       data division.
       working-storage section.
       77 simple-str     pic x(20).
       77 err-str        pic x(50).
      *-----------------------------------------------------------------
       procedure division.
      *    STRING test
           move spaces to simple-str
           string 'data'
             delimited by size
             into simple-str
             on overflow
               move spaces to err-str
               string 'STRING OVERFLOW'
                  delimited by size
                  into err-str
               end-string
               display err-str upon syserr
               end-display
               display '1 failed'
               end-display
             not on overflow
               display '1 passed'
               end-display
           end-string
           if simple-str not = 'data'
             display 'STRING ERROR (1): "' simple-str '"'
             end-display
           end-if
      *
           move spaces to simple-str
           string 'data is too big here...'
             delimited by size
             into simple-str
             on overflow
               display '2 passed'
               end-display
             not on overflow
               display '2 failed'
               end-display
               move spaces to err-str
               string 'missing OVERFLOW'
                  delimited by size
                  into err-str
               end-string
               display err-str upon syserr
               end-display
           end-string
           if simple-str not = 'data is too big here'
             display 'STRING ERROR (2): "' simple-str '"'
             end-display
           end-if
      *
      *    UNSTRING test
           move spaces to simple-str
           unstring 'data'
             into simple-str
             on overflow
               move spaces to err-str
               unstring 'UNSTRING OVERFLOW'
                  into err-str
               end-unstring
               display err-str upon syserr
               end-display
               display '3 failed'
               end-display
             not on overflow
               display '3 passed'
               end-display
           end-unstring
           if simple-str not = 'data'
             display 'UNSTRING ERROR (1): "' simple-str '"'
             end-display
           end-if
      *
           move spaces to simple-str
           unstring 'data is too big here...'
             into simple-str
             on overflow
               display '4 passed'
               end-display
             not on overflow
               display '4 failed'
               end-display
               move spaces to err-str
               string 'missing OVERFLOW'
                  delimited by size
                  into err-str
               end-string
               display err-str upon syserr
               end-display
           end-unstring
           if simple-str not = 'data is too big here'
             display 'UNSTRING ERROR (2): "' simple-str '"'
             end-display
           end-if
      *
           STOP RUN.

