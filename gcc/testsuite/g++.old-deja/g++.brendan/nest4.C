// Build don't link: 
// Special g++ Options: -pedantic-errors
// GROUPS passed nested-classes
class vec {
    class blah { };

    ::vec::blah	satan( 0);// ERROR - .*
    blah	herman( 0);// ERROR - .*
};
