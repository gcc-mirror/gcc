// https://issues.dlang.org/show_bug.cgi?id=18646
class SuperClass {}

class TemplatedClass(T : SuperClass) {}

class A18646 : SuperClass {
    alias T = TemplatedClass!B18646;
}

class B18646 : SuperClass {
    alias T = TemplatedClass!C18646;
}

class C18646 : SuperClass {}
