! { dg-do compile }
! { dg-options "-fdiagnostics-format=json" }

#warning message

! Use dg-regexp to consume the JSON output starting with
! the innermost values, and working outwards.
! We can't rely on any ordering of the keys. 

! { dg-regexp "\"kind\": \"warning\"" }
! { dg-regexp "\"message\": \"#warning message\"" }
! { dg-regexp "\"option\": \"-Wcpp\"" }
! { dg-regexp "\"option_url\": \"\[^\n\r\"\]*#index-Wcpp\"" }

! { dg-regexp "\"caret\": \{" }
! { dg-regexp "\"file\": \"\[^\n\r\"\]*diagnostic-format-json-2.F90\"" }
! { dg-regexp "\"line\": 4" }
! { dg-regexp "\"column\": 2" }

! { dg-regexp "\"finish\": \{" }
! { dg-regexp "\"file\": \"\[^\n\r\"\]*diagnostic-format-json-2.F90\"" }
! { dg-regexp "\"line\": 4" }
! { dg-regexp "\"column\": 8" }

! { dg-regexp "\"locations\": \[\[\{\}, \]*\]" }
! { dg-regexp "\"children\": \[\[\]\[\]\]" }
! { dg-regexp "\[\[\{\}, \]*\]" }
