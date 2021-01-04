// { dg-additional-options -fmodules-ts }

// a bug report ICE with namespace introduced in partition.
export module module1:submodule1;
// { dg-module-cmi module1:submodule1 }

export namespace nmspc{
    class Cl1{
        public:
        Cl1(){}
        int x=14;
    };
}
