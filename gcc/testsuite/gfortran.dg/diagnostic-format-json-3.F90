! { dg-do compile }
! { dg-options "-fdiagnostics-format=json -Werror" }

#warning message

! Use dg-regexp to consume the JSON output starting with
! the innermost values, and working outwards.
! We can't rely on any ordering of the keys.

! { dg-regexp "\"kind\": \"error\"" }
! { dg-regexp "\"message\": \"#warning message\"" }
! { dg-regexp "\"option\": \"-Werror=cpp\"" }

! { dg-regexp "\"caret\": \{" }
! { dg-regexp "\"file\": \"\[^\n\r\"\]*diagnostic-format-json-3.F90\"" }
! { dg-regexp "\"line\": 4" }
! { dg-regexp "\"column\": 2" }

! { dg-regexp "\"finish\": \{" }
! { dg-regexp "\"file\": \"\[^\n\r\"\]*diagnostic-format-json-3.F90\"" }
! { dg-regexp "\"line\": 4" }
! { dg-regexp "\"column\": 8" }

! { dg-regexp "\"locations\": \[\[\{\}, \]*\]" }
! { dg-regexp "\"children\": \[\[\]\[\]\]" }
! { dg-regexp "\[\[\{\}, \]*\]" }
