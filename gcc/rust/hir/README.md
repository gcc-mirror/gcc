# GCCRS HIR

Working with the AST has proved to become difficult. To overcome non lexical scoping
a toplevel scan pass was added to provide lookups for functioins. To get ready for the
gimple conversion pass, type resolution scanned blocks to create the list of locals per
block. Type Conversion had to create awkward getters/setters on LetStmts to have a Type or
InferedType which was more of a hack that what should really be there. ArrayExprs get checked
and create their own type to be checked against a possible LetStmt type. All of these things
started to become hard to manage.

HIR from the RFC defines how they create lookups and IDs for all the nodes which solves the toplevel
scan pass. The lowering to HIR allows for cleanup in how types are resolved. Without using
the HIR and IDs implementing the shadowing rules was going to become very difficult.


## IMPL:

* AST-lower - move down to HIR classes - generate mappings and IDs

* Name Resolution - Check for path segments naming and map to HirIDS
  This should in theory map for example a call expression to already have the HirID to the function ready
  Dyn dispatch may need some help here if its a method the receiver could be bound the local name hirid
  the resoltion would need to be fixed up in type resolution pass
  
* Expand - Gather locals per block and fix up returns is it possible to generate return expressions
  at this pass?
  
* Type Resolution - Port over work from AST Type resolver
  generate mir from this pass?


For now this can then port over to the existing GIMPLE conversion faily easily. But after more
of the core data structures work MIR will be needed for all the glue that will need to be generated.


## Returns

looks like its implemented by an implicit mutable return variable for the function. If were processing
a block expression and the last element on the block is an expression we can try to bind it to the mutable
return variable.
