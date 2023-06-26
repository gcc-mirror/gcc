/* { dg-additional-options "-fdiagnostics-text-art-charset=unicode" } */

#include <stdint.h>

extern int32_t arr_0[0]; /* { dg-message "capacity: 0 bytes" } */
extern int32_t arr_1[1]; /* { dg-message "capacity: 4 bytes" } */
extern int32_t arr_2[2]; /* { dg-message "capacity: 8 bytes" } */
extern int32_t arr_3[3]; /* { dg-message "capacity: 12 bytes" } */
extern int32_t arr_4[4]; /* { dg-message "capacity: 16 bytes" } */

void test_immediately_after (int x)
{
  arr_0[0] = x; /* { dg-warning "buffer overflow" } */
  arr_1[1] = x; /* { dg-warning "buffer overflow" } */
  arr_2[2] = x; /* { dg-warning "buffer overflow" } */
  arr_3[3] = x; /* { dg-warning "buffer overflow" } */
  arr_4[4] = x; /* { dg-warning "buffer overflow" } */
}

/* Expect no diagram for the arr_0 case: there's no valid region 
to write to.  */

/* The arr_1 case.  */
/* { dg-begin-multiline-output "" }

                                      ┌──────────────────────────────────┐
                                      │   write from 'x' (type: 'int')   │
                                      └──────────────────────────────────┘
                                                       │
                                                       │
                                                       v
  ┌──────────────────────────────────┐┌──────────────────────────────────┐
  │               [0]                ││                                  │
  ├──────────────────────────────────┤│        after valid range         │
  │   'arr_1' (type: 'int32_t[1]')   ││                                  │
  └──────────────────────────────────┘└──────────────────────────────────┘
  ├────────────────┬─────────────────┤├────────────────┬─────────────────┤
                   │                                   │
          ╭────────┴────────╮                ╭─────────┴─────────╮
          │capacity: 4 bytes│                │overflow of 4 bytes│
          ╰─────────────────╯                ╰───────────────────╯

   { dg-end-multiline-output "" } */

/* The arr_2 case.  */
/* { dg-begin-multiline-output "" }

                                            ┌────────────────────────────┐
                                            │write from 'x' (type: 'int')│
                                            └────────────────────────────┘
                                                          │
                                                          │
                                                          v
  ┌────────────────────┬───────────────────┐┌────────────────────────────┐
  │        [0]         │        [1]        ││                            │
  ├────────────────────┴───────────────────┤│     after valid range      │
  │      'arr_2' (type: 'int32_t[2]')      ││                            │
  └────────────────────────────────────────┘└────────────────────────────┘
  ├───────────────────┬────────────────────┤├─────────────┬──────────────┤
                      │                                   │
             ╭────────┴────────╮                ╭─────────┴─────────╮
             │capacity: 8 bytes│                │overflow of 4 bytes│
             ╰─────────────────╯                ╰───────────────────╯

   { dg-end-multiline-output "" } */

/* The arr_3 case.  */
// Perhaps we should show [1] rather than ellipsize here.
/* { dg-begin-multiline-output "" }

                                            ┌────────────────────────────┐
                                            │write from 'x' (type: 'int')│
                                            └────────────────────────────┘
                                                          │
                                                          │
                                                          v
  ┌─────────────┬─────────────┬────────────┐┌────────────────────────────┐
  │     [0]     │     ...     │    [2]     ││                            │
  ├─────────────┴─────────────┴────────────┤│     after valid range      │
  │      'arr_3' (type: 'int32_t[3]')      ││                            │
  └────────────────────────────────────────┘└────────────────────────────┘
  ├───────────────────┬────────────────────┤├─────────────┬──────────────┤
                      │                                   │
            ╭─────────┴────────╮                ╭─────────┴─────────╮
            │capacity: 12 bytes│                │overflow of 4 bytes│
            ╰──────────────────╯                ╰───────────────────╯

   { dg-end-multiline-output "" } */

/* The arr_4 case.  */
/* { dg-begin-multiline-output "" }

                                            ┌────────────────────────────┐
                                            │write from 'x' (type: 'int')│
                                            └────────────────────────────┘
                                                          │
                                                          │
                                                          v
  ┌──────────┬──────────────────┬──────────┐┌────────────────────────────┐
  │   [0]    │       ...        │   [3]    ││                            │
  ├──────────┴──────────────────┴──────────┤│     after valid range      │
  │      'arr_4' (type: 'int32_t[4]')      ││                            │
  └────────────────────────────────────────┘└────────────────────────────┘
  ├───────────────────┬────────────────────┤├─────────────┬──────────────┤
                      │                                   │
            ╭─────────┴────────╮                ╭─────────┴─────────╮
            │capacity: 16 bytes│                │overflow of 4 bytes│
            ╰──────────────────╯                ╰───────────────────╯

   { dg-end-multiline-output "" } */
