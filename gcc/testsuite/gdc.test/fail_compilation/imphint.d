/*
TEST_OUTPUT:
---
fail_compilation/imphint.d(60): Error: `printf` is not defined, perhaps `import core.stdc.stdio;` is needed?
fail_compilation/imphint.d(61): Error: `writeln` is not defined, perhaps `import std.stdio;` is needed?
fail_compilation/imphint.d(62): Error: `sin` is not defined, perhaps `import std.math;` is needed?
fail_compilation/imphint.d(63): Error: `cos` is not defined, perhaps `import std.math;` is needed?
fail_compilation/imphint.d(64): Error: `sqrt` is not defined, perhaps `import std.math;` is needed?
fail_compilation/imphint.d(65): Error: `fabs` is not defined, perhaps `import std.math;` is needed?
fail_compilation/imphint.d(68): Error: `AliasSeq` is not defined, perhaps `import std.meta;` is needed?
fail_compilation/imphint.d(69): Error: `appender` is not defined, perhaps `import std.array;` is needed?
fail_compilation/imphint.d(70): Error: `array` is not defined, perhaps `import std.array;` is needed?
fail_compilation/imphint.d(71): Error: `calloc` is not defined, perhaps `import core.stdc.stdlib;` is needed?
fail_compilation/imphint.d(72): Error: `chdir` is not defined, perhaps `import std.file;` is needed?
fail_compilation/imphint.d(73): Error: `dirEntries` is not defined, perhaps `import std.file;` is needed?
fail_compilation/imphint.d(74): Error: `drop` is not defined, perhaps `import std.range;` is needed?
fail_compilation/imphint.d(75): Error: `each` is not defined, perhaps `import std.algorithm;` is needed?
fail_compilation/imphint.d(76): Error: `empty` is not defined, perhaps `import std.range;` is needed?
fail_compilation/imphint.d(77): Error: `enumerate` is not defined, perhaps `import std.range;` is needed?
fail_compilation/imphint.d(78): Error: `endsWith` is not defined, perhaps `import std.algorithm;` is needed?
fail_compilation/imphint.d(79): Error: `enforce` is not defined, perhaps `import std.exception;` is needed?
fail_compilation/imphint.d(80): Error: `equal` is not defined, perhaps `import std.algorithm;` is needed?
fail_compilation/imphint.d(81): Error: `exists` is not defined, perhaps `import std.file;` is needed?
fail_compilation/imphint.d(82): Error: `filter` is not defined, perhaps `import std.algorithm;` is needed?
fail_compilation/imphint.d(83): Error: `format` is not defined, perhaps `import std.format;` is needed?
fail_compilation/imphint.d(84): Error: `free` is not defined, perhaps `import core.stdc.stdlib;` is needed?
fail_compilation/imphint.d(85): Error: `front` is not defined, perhaps `import std.range;` is needed?
fail_compilation/imphint.d(86): Error: `iota` is not defined, perhaps `import std.range;` is needed?
fail_compilation/imphint.d(87): Error: `isDir` is not defined, perhaps `import std.file;` is needed?
fail_compilation/imphint.d(88): Error: `isFile` is not defined, perhaps `import std.file;` is needed?
fail_compilation/imphint.d(89): Error: `join` is not defined, perhaps `import std.array;` is needed?
fail_compilation/imphint.d(90): Error: `joiner` is not defined, perhaps `import std.algorithm;` is needed?
fail_compilation/imphint.d(91): Error: `malloc` is not defined, perhaps `import core.stdc.stdlib;` is needed?
fail_compilation/imphint.d(92): Error: `map` is not defined, perhaps `import std.algorithm;` is needed?
fail_compilation/imphint.d(93): Error: `max` is not defined, perhaps `import std.algorithm;` is needed?
fail_compilation/imphint.d(94): Error: `min` is not defined, perhaps `import std.algorithm;` is needed?
fail_compilation/imphint.d(95): Error: `mkdir` is not defined, perhaps `import std.file;` is needed?
fail_compilation/imphint.d(96): Error: `popFront` is not defined, perhaps `import std.range;` is needed?
fail_compilation/imphint.d(97): Error: `realloc` is not defined, perhaps `import core.stdc.stdlib;` is needed?
fail_compilation/imphint.d(98): Error: `replace` is not defined, perhaps `import std.array;` is needed?
fail_compilation/imphint.d(99): Error: `rmdir` is not defined, perhaps `import std.file;` is needed?
fail_compilation/imphint.d(100): Error: `sort` is not defined, perhaps `import std.algorithm;` is needed?
fail_compilation/imphint.d(101): Error: `split` is not defined, perhaps `import std.array;` is needed?
fail_compilation/imphint.d(102): Error: `startsWith` is not defined, perhaps `import std.algorithm;` is needed?
fail_compilation/imphint.d(103): Error: `take` is not defined, perhaps `import std.range;` is needed?
fail_compilation/imphint.d(104): Error: `text` is not defined, perhaps `import std.conv;` is needed?
fail_compilation/imphint.d(105): Error: `to` is not defined, perhaps `import std.conv;` is needed?
fail_compilation/imphint.d(107): Error: `InterpolationHeader` is not defined, perhaps `import core.interpolation;` ?
fail_compilation/imphint.d(108): Error: template `heresy` is not callable using argument types `!()(InterpolationHeader, InterpolationFooter)`
fail_compilation/imphint.d(107):        Candidate is: `heresy(Args...)(InterpolationHeader header, Args args, InterpolationFooter footer)`
---
*/





void foo()
{
    printf("hello world\n");
    writeln("hello world\n");
    sin(3.6);
    cos(1.2);
    sqrt(2.0);
    fabs(-3);


    AliasSeq();
    appender();
    array();
    calloc();
    chdir();
    dirEntries();
    drop();
    each();
    empty();
    enumerate();
    endsWith();
    enforce();
    equal();
    exists();
    filter();
    format();
    free();
    front();
    iota();
    isDir();
    isFile();
    join();
    joiner();
    malloc();
    map();
    max();
    min();
    mkdir();
    popFront();
    realloc();
    replace();
    rmdir();
    sort();
    split();
    startsWith();
    take();
    text();
    to();

    void heresy(Args...)(InterpolationHeader header, Args args, InterpolationFooter footer) {}
    heresy(i"");
}
