# Borrow-checker Development Notes

## Testing BIR building

There is no way to test BIR building directly, since it is a dead branch of the compilation pipeline.
The only way to verify its generations is through manual inspection.
The best way to inspect the BIR is to compare it with rustc's MIR.

The following command will compile a rust file into a library and dump its MIR:

```shell
rustc --crate-type=lib -A dead_code -A unused -Z dump-mir="" <file>
```

The MIR dump directory `mir_dump` contains a dump before and after each MIR pass.
We are interested in the one used for borrow-checking, which is called `<crate>.<function>.002-000.analysis.after.mir`.

BIR dump is emitted to a `bir_dump` directory. With the following naming scheme: `<crate>.<function>.bir.dump`.

At this point, MIR dump contains helper constructions that BIR does not contain yet (like storage live/dead annotations). To remove them from the MIR dump, run the following command:

```shell
awk -i inplace '!/^\s*(\/\/|StorageLive|StorageDead|FakeRead)/' mir_dump/*
```

To get the BIR dump into a similar format, run the following command:

```shell
./crab1 <file> -frust-incomplete-and-experimental-compiler-do-not-use -frust-borrowcheck -frust-dump-bir -frust-compile-until=compilation
```


## TODO

- scope handling, cleanup
- switch coercions to adjustments from typechecking
- operator overloading
- match selection
- let without an initializer
- lifetime parameters