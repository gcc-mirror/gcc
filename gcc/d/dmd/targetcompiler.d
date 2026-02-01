/**
 * Encapsulates the vagaries of which of the three D compilers (gdc, ldc or dmd) is being built
 *
 * Copyright:   Copyright (C) 1999-2025 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/compiler/src/dmd/mars.d, _targetcompiler.d)
 * Documentation:  https://dlang.org/phobos/dmd_targetcompiler.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/compiler/src/dmd/targetcompiler.d
 */

module dmd.targetcompiler;

version (IN_GCC) {}        // compiler is being built with gdc
else version (IN_LLVM) {}  // compiler is being built with ldc
else version = MARS;       // default means compiler is built with Digital Mars compiler (DMD)

/***************************
 */

version (IN_GCC)
{
    enum TargetCompiler = "GNU D";    // compiler being built
    enum SwitchPrefix = "f";          // prefix to switch
    enum SwitchVariadic = "fno-rtti"; // disables D-style variadic functions
    enum const(char)* SwitchExceptions = "fno-exceptions"; // switch that disables exceptions
    enum const(char)* SwitchScopeGuard = "fno-exceptions"; // switch that disables exceptions
}
else version (IN_LLVM)
{
    enum TargetCompiler = "LDC";
    enum SwitchPrefix = "";
    enum SwitchVariadic = "betterC";
    enum const(char)* SwitchExceptions = null;
    enum const(char)* SwitchScopeGuard = "betterC";
}
else version (MARS)
{
    enum TargetCompiler = "Digital Mars D";
    enum SwitchPrefix = "";
    enum SwitchVariadic = "betterC";
    enum const(char)* SwitchExceptions = null;
    enum const(char)* SwitchScopeGuard = "betterC";
}
else
    static assert(0, "unknown compiler being built");


/**********************************
 * Extra Fields for VarDeclaration
 */
version (IN_GCC)
{
    mixin template VarDeclarationExtra() { }
}
else version (IN_LLVM)
{
    mixin template VarDeclarationExtra() { }
}
else version (MARS)
{
    mixin template VarDeclarationExtra()
    {
        bool inClosure;         /// is inserted into a GC allocated closure
        bool inAlignSection;    /// is inserted into an aligned section on stack
    }
}
else
    static assert(0, "unknown compiler being built");

/**********************************
 * Extra Fields for FuncDeclaration
 */
version (IN_GCC)
{
    mixin template FuncDeclarationExtra() { }
}
else version (IN_LLVM)
{
    mixin template FuncDeclarationExtra() { }
}
else version (MARS)
{
    mixin template FuncDeclarationExtra()
    {
        VarDeclarations* alignSectionVars;  /// local variables with alignment needs larger than stackAlign
        import dmd.backend.cc : Symbol;
        Symbol* salignSection;              /// pointer to aligned section, if any
    }
}
else
    static assert(0, "unknown compiler being built");

/**********************************
 * Extra Fields for FuncDeclaration
 */
version (IN_GCC)
{
    mixin template alignSectionVarsExtra() { void doAlign() { } }
}
else version (IN_LLVM)
{
    mixin template alignSectionVarsExtra() { void doAlign() { } }
}
else version (MARS)
{
    /* If the alignment of a stack local is greater than the stack alignment,
     * note it in the enclosing function's alignSectionVars
     */
    mixin template alignSectionVarsExtra()
    {
        void doAlign()
        {
            if (!dsym.alignment.isDefault() && sc.func &&
                dsym.alignment.get() > target.stackAlign() &&
                sc.func && !dsym.isDataseg() && !dsym.isParameter() && !dsym.isField())
            {
                auto fd = sc.func;
                if (!fd.alignSectionVars)
                    fd.alignSectionVars = new VarDeclarations();
                fd.alignSectionVars.push(dsym);
            }
        }
    }
}
else
    static assert(0, "unknown compiler being built");


/* Returns: true if v is in an align section
 */
mixin template alignSectionVarsContains()
{
    version (IN_GCC)
    {
        bool isAlignSectionVar(VarDeclaration v) { return false; }
    }
    else version (IN_LLVM)
    {
        bool isAlignSectionVar(VarDeclaration v) { return false; }
    }
    else version (MARS)
    {
        bool isAlignSectionVar(VarDeclaration v)
        {
            return fd.alignSectionVars && (*fd.alignSectionVars).contains(v);
        }
    }
    else
        static assert(0, "unknown compiler being built");
}

/* Returns: true if ansi color is to be used
 */

mixin template UseAnsiColors()
{
    bool useAnsiColors()
    {
        version (IN_GCC)
        {
            return false;
        }
        else version (IN_LLVM)
        {
            import dmd.console : detectTerminal;
            return detectTerminal();
        }
        else version (MARS)
        {
            // -color=auto is the default value
            import dmd.console : detectTerminal, detectColorPreference;
            return detectTerminal() && detectColorPreference();
        }
        else
            static assert(0, "unknown compiler being built");
    }
}

/******************************************
 * Let user know object.d cannot be found.
 * Parameters:
 *      loc = location of error
 *      id = name of symbol that cannot be found
 *      configFile = configuration file name
 *      eSink = where to send the error messages
 */

mixin template HostObjectNotFound()
{
    void hostObjectNotFound(Loc loc, const(char)* id, const(char)[] configFile, ErrorSink eSink)
    {
        eSink.error(loc, "`%s` not found. object.d may be incorrectly installed or corrupt.", id);
        version (IN_GCC)
        {
        }
        else version (IN_LLVM)
        {
            eSink.errorSupplemental(loc, "ldc2 might not be correctly installed.");
            eSink.errorSupplemental(loc, "Please check your ldc2.conf configuration file.");
            eSink.errorSupplemental(loc, "Installation instructions can be found at http://wiki.dlang.org/LDC.");
        }
        else version (MARS)
        {
            eSink.errorSupplemental(loc, "dmd might not be correctly installed. Run 'dmd -man' for installation instructions.");
            eSink.errorSupplemental(loc, "config file: %.*s", configFile.length, configFile.ptr);
        }
        else
            static assert(0, "unknown compiler being built");
    }
}
