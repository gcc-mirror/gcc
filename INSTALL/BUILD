
                            Building egcs-1.0.3
                                      
   Now that egcs is configured, you are ready to build the compiler and
   runtime libraries.
   
   We highly recommend that egcs be built using gnu-make; other versions
   make work, then again they might not. To be safe build with gnu-make.
   
   Building a native compiler
   
   For a native build issue the command "make bootstrap". This will build
   the entire egcs compiler system, which includes the following steps:
     * Build host tools necessary to build the compiler such as texinfo,
       bison, gperf.
     * Build target tools for use by the compiler such as gas, gld, and
       binutils.
     * Perform a 3-stage bootstrap of the compiler.
     * Perform a comparison test of the stage2 and stage3 compilers.
     * Build runtime libraries using the stage3 compiler from the
       previous step.
       
   If you are short on disk space you might consider "make
   bootstrap-lean" instead. This is identical to "make bootstrap" except
   that object files from the stage1 and stage2 of the 3-stage bootstrap
   of the compiler are deleted as soon as they are no longer needed.
   
   Building a cross compiler
   
   We recommend reading the [1]crossgcc FAQ for information about
   building cross compilers.
   
   For a cross build, issue the command "make cross", which performs the
   following steps:
     * Build host tools necessary to build the compiler such as texinfo,
       bison, gperf.
     * Build target tools for use by the compiler such as gas, gld, and
       binutils.
     * Build the compiler (single stage only).
     * Build runtime libraries using the compiler from the previous step.
       
   Note that if an error occurs in any step the make process will exit.
   
     _________________________________________________________________
                                      
   Last modified on Jan 2, 1998.

References

   1. ftp://ftp.cygnus.com/pub/embedded/crossgcc/FAQ-0.8.1
