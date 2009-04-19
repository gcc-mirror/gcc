/* Test redefinitions differing only in the spelling of paste and
   stringify tokens, whitespace around them, or the number of
   consecutive paste tokens.  */
/* { dg-do preprocess } */
/* { dg-options "" } */

#define str(x) #x /* { dg-message "previous definition" } */
#define str(x) %: x /* { dg-warning "redefined" } */
#undef str
#define str(x) #x /* { dg-message "previous definition" } */
#define str(x) # x /* { dg-warning "redefined" } */
#undef str
#define str(x) #x /* { dg-message "previous definition" } */
#define str(x) %: x /* { dg-warning "redefined" } */
#undef str
#define str(x) %:x /* { dg-message "previous definition" } */
#define str(x) #x /* { dg-warning "redefined" } */
#undef str
#define str(x) %:x /* { dg-message "previous definition" } */
#define str(x) %: x /* { dg-warning "redefined" } */
#undef str
#define str(x) %:x /* { dg-message "previous definition" } */
#define str(x) # x /* { dg-warning "redefined" } */
#undef str
#define str(x) %:x /* { dg-message "previous definition" } */
#define str(x) %: x /* { dg-warning "redefined" } */
#undef str
#define str(x) # x /* { dg-message "previous definition" } */
#define str(x) #x /* { dg-warning "redefined" } */
#undef str
#define str(x) # x /* { dg-message "previous definition" } */
#define str(x) %: x /* { dg-warning "redefined" } */
#undef str
#define str(x) # x /* { dg-message "previous definition" } */
#define str(x) %: x /* { dg-warning "redefined" } */
#undef str
#define str(x) %: x /* { dg-message "previous definition" } */
#define str(x) #x /* { dg-warning "redefined" } */
#undef str
#define str(x) %: x /* { dg-message "previous definition" } */
#define str(x) # x /* { dg-warning "redefined" } */
#undef str

#define str(x) #x
#define str(x) #x
#undef str
#define str(x) # x
#define str(x) # x
#undef str
#define str(x) %: x
#define str(x) %: x
#undef str
#define str(x) %: x
#define str(x) %: x
#undef str

#define astr(x) a#x /* { dg-message "previous definition" } */
#define astr(x) a%: x /* { dg-warning "redefined" } */
#undef astr
#define astr(x) a#x /* { dg-message "previous definition" } */
#define astr(x) a# x /* { dg-warning "redefined" } */
#undef astr
#define astr(x) a#x /* { dg-message "previous definition" } */
#define astr(x) a%: x /* { dg-warning "redefined" } */
#undef astr
#define astr(x) a#x /* { dg-message "previous definition" } */
#define astr(x) a #x /* { dg-warning "redefined" } */
#undef astr
#define astr(x) a#x /* { dg-message "previous definition" } */
#define astr(x) a %:x /* { dg-warning "redefined" } */
#undef astr
#define astr(x) a#x /* { dg-message "previous definition" } */
#define astr(x) a # x /* { dg-warning "redefined" } */
#undef astr
#define astr(x) a#x /* { dg-message "previous definition" } */
#define astr(x) a %: x /* { dg-warning "redefined" } */
#undef astr
#define astr(x) a%:x /* { dg-message "previous definition" } */
#define astr(x) a#x /* { dg-warning "redefined" } */
#undef astr
#define astr(x) a%:x /* { dg-message "previous definition" } */
#define astr(x) a%: x /* { dg-warning "redefined" } */
#undef astr
#define astr(x) a%:x /* { dg-message "previous definition" } */
#define astr(x) a# x /* { dg-warning "redefined" } */
#undef astr
#define astr(x) a%:x /* { dg-message "previous definition" } */
#define astr(x) a%: x /* { dg-warning "redefined" } */
#undef astr
#define astr(x) a%:x /* { dg-message "previous definition" } */
#define astr(x) a #x /* { dg-warning "redefined" } */
#undef astr
#define astr(x) a%:x /* { dg-message "previous definition" } */
#define astr(x) a %:x /* { dg-warning "redefined" } */
#undef astr
#define astr(x) a%:x /* { dg-message "previous definition" } */
#define astr(x) a # x /* { dg-warning "redefined" } */
#undef astr
#define astr(x) a%:x /* { dg-message "previous definition" } */
#define astr(x) a %: x /* { dg-warning "redefined" } */
#undef astr
#define astr(x) a# x /* { dg-message "previous definition" } */
#define astr(x) a#x /* { dg-warning "redefined" } */
#undef astr
#define astr(x) a# x /* { dg-message "previous definition" } */
#define astr(x) a%: x /* { dg-warning "redefined" } */
#undef astr
#define astr(x) a# x /* { dg-message "previous definition" } */
#define astr(x) a%: x /* { dg-warning "redefined" } */
#undef astr
#define astr(x) a# x /* { dg-message "previous definition" } */
#define astr(x) a #x /* { dg-warning "redefined" } */
#undef astr
#define astr(x) a# x /* { dg-message "previous definition" } */
#define astr(x) a %:x /* { dg-warning "redefined" } */
#undef astr
#define astr(x) a# x /* { dg-message "previous definition" } */
#define astr(x) a # x /* { dg-warning "redefined" } */
#undef astr
#define astr(x) a# x /* { dg-message "previous definition" } */
#define astr(x) a %: x /* { dg-warning "redefined" } */
#undef astr
#define astr(x) a%: x /* { dg-message "previous definition" } */
#define astr(x) a#x /* { dg-warning "redefined" } */
#undef astr
#define astr(x) a%: x /* { dg-message "previous definition" } */
#define astr(x) a# x /* { dg-warning "redefined" } */
#undef astr
#define astr(x) a%: x /* { dg-message "previous definition" } */
#define astr(x) a #x /* { dg-warning "redefined" } */
#undef astr
#define astr(x) a%: x /* { dg-message "previous definition" } */
#define astr(x) a %:x /* { dg-warning "redefined" } */
#undef astr
#define astr(x) a%: x /* { dg-message "previous definition" } */
#define astr(x) a # x /* { dg-warning "redefined" } */
#undef astr
#define astr(x) a%: x /* { dg-message "previous definition" } */
#define astr(x) a %: x /* { dg-warning "redefined" } */
#undef astr
#define astr(x) a #x /* { dg-message "previous definition" } */
#define astr(x) a#x /* { dg-warning "redefined" } */
#undef astr
#define astr(x) a #x /* { dg-message "previous definition" } */
#define astr(x) a%: x /* { dg-warning "redefined" } */
#undef astr
#define astr(x) a #x /* { dg-message "previous definition" } */
#define astr(x) a# x /* { dg-warning "redefined" } */
#undef astr
#define astr(x) a #x /* { dg-message "previous definition" } */
#define astr(x) a%: x /* { dg-warning "redefined" } */
#undef astr
#define astr(x) a #x /* { dg-message "previous definition" } */
#define astr(x) a %:x /* { dg-warning "redefined" } */
#undef astr
#define astr(x) a #x /* { dg-message "previous definition" } */
#define astr(x) a # x /* { dg-warning "redefined" } */
#undef astr
#define astr(x) a #x /* { dg-message "previous definition" } */
#define astr(x) a %: x /* { dg-warning "redefined" } */
#undef astr
#define astr(x) a %:x /* { dg-message "previous definition" } */
#define astr(x) a#x /* { dg-warning "redefined" } */
#undef astr
#define astr(x) a %:x /* { dg-message "previous definition" } */
#define astr(x) a%: x /* { dg-warning "redefined" } */
#undef astr
#define astr(x) a %:x /* { dg-message "previous definition" } */
#define astr(x) a# x /* { dg-warning "redefined" } */
#undef astr
#define astr(x) a %:x /* { dg-message "previous definition" } */
#define astr(x) a%: x /* { dg-warning "redefined" } */
#undef astr
#define astr(x) a %:x /* { dg-message "previous definition" } */
#define astr(x) a #x /* { dg-warning "redefined" } */
#undef astr
#define astr(x) a %:x /* { dg-message "previous definition" } */
#define astr(x) a # x /* { dg-warning "redefined" } */
#undef astr
#define astr(x) a %:x /* { dg-message "previous definition" } */
#define astr(x) a %: x /* { dg-warning "redefined" } */
#undef astr
#define astr(x) a # x /* { dg-message "previous definition" } */
#define astr(x) a#x /* { dg-warning "redefined" } */
#undef astr
#define astr(x) a # x /* { dg-message "previous definition" } */
#define astr(x) a%: x /* { dg-warning "redefined" } */
#undef astr
#define astr(x) a # x /* { dg-message "previous definition" } */
#define astr(x) a# x /* { dg-warning "redefined" } */
#undef astr
#define astr(x) a # x /* { dg-message "previous definition" } */
#define astr(x) a%: x /* { dg-warning "redefined" } */
#undef astr
#define astr(x) a # x /* { dg-message "previous definition" } */
#define astr(x) a #x /* { dg-warning "redefined" } */
#undef astr
#define astr(x) a # x /* { dg-message "previous definition" } */
#define astr(x) a %:x /* { dg-warning "redefined" } */
#undef astr
#define astr(x) a # x /* { dg-message "previous definition" } */
#define astr(x) a %: x /* { dg-warning "redefined" } */
#undef astr
#define astr(x) a %: x /* { dg-message "previous definition" } */
#define astr(x) a#x /* { dg-warning "redefined" } */
#undef astr
#define astr(x) a %: x /* { dg-message "previous definition" } */
#define astr(x) a%: x /* { dg-warning "redefined" } */
#undef astr
#define astr(x) a %: x /* { dg-message "previous definition" } */
#define astr(x) a# x /* { dg-warning "redefined" } */
#undef astr
#define astr(x) a %: x /* { dg-message "previous definition" } */
#define astr(x) a%: x /* { dg-warning "redefined" } */
#undef astr
#define astr(x) a %: x /* { dg-message "previous definition" } */
#define astr(x) a #x /* { dg-warning "redefined" } */
#undef astr
#define astr(x) a %: x /* { dg-message "previous definition" } */
#define astr(x) a %:x /* { dg-warning "redefined" } */
#undef astr
#define astr(x) a %: x /* { dg-message "previous definition" } */
#define astr(x) a # x /* { dg-warning "redefined" } */
#undef astr

#define astr(x) a#x
#define astr(x) a#x
#undef astr
#define astr(x) a# x
#define astr(x) a# x
#undef astr
#define astr(x) a%: x
#define astr(x) a%: x
#undef astr
#define astr(x) a%: x
#define astr(x) a%: x
#undef astr
#define astr(x) a #x
#define astr(x) a #x
#undef astr
#define astr(x) a %:x
#define astr(x) a %:x
#undef astr
#define astr(x) a # x
#define astr(x) a # x
#undef astr
#define astr(x) a %: x
#define astr(x) a %: x
#undef astr

#define cat(x,y) x##y /* { dg-message "previous definition" } */
#define cat(x,y) x%:%: y /* { dg-warning "redefined" } */
#undef cat
#define cat(x,y) x##y /* { dg-message "previous definition" } */
#define cat(x,y) x## y /* { dg-warning "redefined" } */
#undef cat
#define cat(x,y) x##y /* { dg-message "previous definition" } */
#define cat(x,y) x%:%: y /* { dg-warning "redefined" } */
#undef cat
#define cat(x,y) x##y /* { dg-message "previous definition" } */
#define cat(x,y) x ##y /* { dg-warning "redefined" } */
#undef cat
#define cat(x,y) x##y /* { dg-message "previous definition" } */
#define cat(x,y) x %:%:y /* { dg-warning "redefined" } */
#undef cat
#define cat(x,y) x##y /* { dg-message "previous definition" } */
#define cat(x,y) x ## y /* { dg-warning "redefined" } */
#undef cat
#define cat(x,y) x##y /* { dg-message "previous definition" } */
#define cat(x,y) x %:%: y /* { dg-warning "redefined" } */
#undef cat
#define cat(x,y) x%:%:y /* { dg-message "previous definition" } */
#define cat(x,y) x##y /* { dg-warning "redefined" } */
#undef cat
#define cat(x,y) x%:%:y /* { dg-message "previous definition" } */
#define cat(x,y) x%:%: y /* { dg-warning "redefined" } */
#undef cat
#define cat(x,y) x%:%:y /* { dg-message "previous definition" } */
#define cat(x,y) x## y /* { dg-warning "redefined" } */
#undef cat
#define cat(x,y) x%:%:y /* { dg-message "previous definition" } */
#define cat(x,y) x%:%: y /* { dg-warning "redefined" } */
#undef cat
#define cat(x,y) x%:%:y /* { dg-message "previous definition" } */
#define cat(x,y) x ##y /* { dg-warning "redefined" } */
#undef cat
#define cat(x,y) x%:%:y /* { dg-message "previous definition" } */
#define cat(x,y) x %:%:y /* { dg-warning "redefined" } */
#undef cat
#define cat(x,y) x%:%:y /* { dg-message "previous definition" } */
#define cat(x,y) x ## y /* { dg-warning "redefined" } */
#undef cat
#define cat(x,y) x%:%:y /* { dg-message "previous definition" } */
#define cat(x,y) x %:%: y /* { dg-warning "redefined" } */
#undef cat
#define cat(x,y) x## y /* { dg-message "previous definition" } */
#define cat(x,y) x##y /* { dg-warning "redefined" } */
#undef cat
#define cat(x,y) x## y /* { dg-message "previous definition" } */
#define cat(x,y) x%:%: y /* { dg-warning "redefined" } */
#undef cat
#define cat(x,y) x## y /* { dg-message "previous definition" } */
#define cat(x,y) x%:%: y /* { dg-warning "redefined" } */
#undef cat
#define cat(x,y) x## y /* { dg-message "previous definition" } */
#define cat(x,y) x ##y /* { dg-warning "redefined" } */
#undef cat
#define cat(x,y) x## y /* { dg-message "previous definition" } */
#define cat(x,y) x %:%:y /* { dg-warning "redefined" } */
#undef cat
#define cat(x,y) x## y /* { dg-message "previous definition" } */
#define cat(x,y) x ## y /* { dg-warning "redefined" } */
#undef cat
#define cat(x,y) x## y /* { dg-message "previous definition" } */
#define cat(x,y) x %:%: y /* { dg-warning "redefined" } */
#undef cat
#define cat(x,y) x%:%: y /* { dg-message "previous definition" } */
#define cat(x,y) x##y /* { dg-warning "redefined" } */
#undef cat
#define cat(x,y) x%:%: y /* { dg-message "previous definition" } */
#define cat(x,y) x## y /* { dg-warning "redefined" } */
#undef cat
#define cat(x,y) x%:%: y /* { dg-message "previous definition" } */
#define cat(x,y) x ##y /* { dg-warning "redefined" } */
#undef cat
#define cat(x,y) x%:%: y /* { dg-message "previous definition" } */
#define cat(x,y) x %:%:y /* { dg-warning "redefined" } */
#undef cat
#define cat(x,y) x%:%: y /* { dg-message "previous definition" } */
#define cat(x,y) x ## y /* { dg-warning "redefined" } */
#undef cat
#define cat(x,y) x%:%: y /* { dg-message "previous definition" } */
#define cat(x,y) x %:%: y /* { dg-warning "redefined" } */
#undef cat
#define cat(x,y) x ##y /* { dg-message "previous definition" } */
#define cat(x,y) x##y /* { dg-warning "redefined" } */
#undef cat
#define cat(x,y) x ##y /* { dg-message "previous definition" } */
#define cat(x,y) x%:%: y /* { dg-warning "redefined" } */
#undef cat
#define cat(x,y) x ##y /* { dg-message "previous definition" } */
#define cat(x,y) x## y /* { dg-warning "redefined" } */
#undef cat
#define cat(x,y) x ##y /* { dg-message "previous definition" } */
#define cat(x,y) x%:%: y /* { dg-warning "redefined" } */
#undef cat
#define cat(x,y) x ##y /* { dg-message "previous definition" } */
#define cat(x,y) x %:%:y /* { dg-warning "redefined" } */
#undef cat
#define cat(x,y) x ##y /* { dg-message "previous definition" } */
#define cat(x,y) x ## y /* { dg-warning "redefined" } */
#undef cat
#define cat(x,y) x ##y /* { dg-message "previous definition" } */
#define cat(x,y) x %:%: y /* { dg-warning "redefined" } */
#undef cat
#define cat(x,y) x %:%:y /* { dg-message "previous definition" } */
#define cat(x,y) x##y /* { dg-warning "redefined" } */
#undef cat
#define cat(x,y) x %:%:y /* { dg-message "previous definition" } */
#define cat(x,y) x%:%: y /* { dg-warning "redefined" } */
#undef cat
#define cat(x,y) x %:%:y /* { dg-message "previous definition" } */
#define cat(x,y) x## y /* { dg-warning "redefined" } */
#undef cat
#define cat(x,y) x %:%:y /* { dg-message "previous definition" } */
#define cat(x,y) x%:%: y /* { dg-warning "redefined" } */
#undef cat
#define cat(x,y) x %:%:y /* { dg-message "previous definition" } */
#define cat(x,y) x ##y /* { dg-warning "redefined" } */
#undef cat
#define cat(x,y) x %:%:y /* { dg-message "previous definition" } */
#define cat(x,y) x ## y /* { dg-warning "redefined" } */
#undef cat
#define cat(x,y) x %:%:y /* { dg-message "previous definition" } */
#define cat(x,y) x %:%: y /* { dg-warning "redefined" } */
#undef cat
#define cat(x,y) x ## y /* { dg-message "previous definition" } */
#define cat(x,y) x##y /* { dg-warning "redefined" } */
#undef cat
#define cat(x,y) x ## y /* { dg-message "previous definition" } */
#define cat(x,y) x%:%: y /* { dg-warning "redefined" } */
#undef cat
#define cat(x,y) x ## y /* { dg-message "previous definition" } */
#define cat(x,y) x## y /* { dg-warning "redefined" } */
#undef cat
#define cat(x,y) x ## y /* { dg-message "previous definition" } */
#define cat(x,y) x%:%: y /* { dg-warning "redefined" } */
#undef cat
#define cat(x,y) x ## y /* { dg-message "previous definition" } */
#define cat(x,y) x ##y /* { dg-warning "redefined" } */
#undef cat
#define cat(x,y) x ## y /* { dg-message "previous definition" } */
#define cat(x,y) x %:%:y /* { dg-warning "redefined" } */
#undef cat
#define cat(x,y) x ## y /* { dg-message "previous definition" } */
#define cat(x,y) x %:%: y /* { dg-warning "redefined" } */
#undef cat
#define cat(x,y) x %:%: y /* { dg-message "previous definition" } */
#define cat(x,y) x##y /* { dg-warning "redefined" } */
#undef cat
#define cat(x,y) x %:%: y /* { dg-message "previous definition" } */
#define cat(x,y) x%:%: y /* { dg-warning "redefined" } */
#undef cat
#define cat(x,y) x %:%: y /* { dg-message "previous definition" } */
#define cat(x,y) x## y /* { dg-warning "redefined" } */
#undef cat
#define cat(x,y) x %:%: y /* { dg-message "previous definition" } */
#define cat(x,y) x%:%: y /* { dg-warning "redefined" } */
#undef cat
#define cat(x,y) x %:%: y /* { dg-message "previous definition" } */
#define cat(x,y) x ##y /* { dg-warning "redefined" } */
#undef cat
#define cat(x,y) x %:%: y /* { dg-message "previous definition" } */
#define cat(x,y) x %:%:y /* { dg-warning "redefined" } */
#undef cat
#define cat(x,y) x %:%: y /* { dg-message "previous definition" } */
#define cat(x,y) x ## y /* { dg-warning "redefined" } */
#undef cat

#define cat(x,y) x##y
#define cat(x,y) x##y
#undef cat
#define cat(x,y) x## y
#define cat(x,y) x## y
#undef cat
#define cat(x,y) x%:%: y
#define cat(x,y) x%:%: y
#undef cat
#define cat(x,y) x%:%: y
#define cat(x,y) x%:%: y
#undef cat
#define cat(x,y) x ##y
#define cat(x,y) x ##y
#undef cat
#define cat(x,y) x %:%:y
#define cat(x,y) x %:%:y
#undef cat
#define cat(x,y) x ## y
#define cat(x,y) x ## y
#undef cat
#define cat(x,y) x %:%: y
#define cat(x,y) x %:%: y
#undef cat

#define cat3(x,y,z) x##y##z /* { dg-message "previous definition" } */
#define cat3(x,y,z) x##y####z /* { dg-warning "redefined" } */
#undef cat3

#define cat3(x,y,z) x##y####z /* { dg-message "previous definition" } */
#define cat3(x,y,z) x####y##z /* { dg-warning "redefined" } */
#undef cat3

#define cat3(x,y,z) x##y####z /* { dg-message "previous definition" } */
#define cat3(x,y,z) x##y## ##z /* { dg-warning "redefined" } */
#undef cat3

#define cat3(x,y,z) x##y####z /* { dg-message "previous definition" } */
#define cat3(x,y,z) x##y##%:%:z /* { dg-warning "redefined" } */
#undef cat3

#define cat3(x,y,z) x##y######## ####z /* { dg-message "previous definition" } */
#define cat3(x,y,z) x##y############z /* { dg-warning "redefined" } */
#undef cat3

#define cat3(x,y,z) x##y############z /* { dg-message "previous definition" } */
#define cat3(x,y,z) x##y########%:%:##z /* { dg-warning "redefined" } */
#undef cat3

#define cat3(x,y,z) x##y##z
#define cat3(x,y,z) x##y##z
#undef cat3

#define cat3(x,y,z) x##y####z
#define cat3(x,y,z) x##y####z
#undef cat3

#define cat3(x,y,z) x####y##z
#define cat3(x,y,z) x####y##z
#undef cat3

#define cat3(x,y,z) x##y## ##z
#define cat3(x,y,z) x##y## ##z
#undef cat3

#define cat3(x,y,z) x##y##%:%:z
#define cat3(x,y,z) x##y##%:%:z
#undef cat3

#define cat3(x,y,z) x##y######## ####z
#define cat3(x,y,z) x##y######## ####z
#undef cat3

#define cat3(x,y,z) x##y############z
#define cat3(x,y,z) x##y############z
#undef cat3

#define cat3(x,y,z) x##y########%:%:##z
#define cat3(x,y,z) x##y########%:%:##z
#undef cat3
