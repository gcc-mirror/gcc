// { dg-additional-options -MD }
// { dg-additional-options "-MF depflags-3.ddi" }
// { dg-additional-options -fdeps-file=depflags-3.ddi }
// { dg-additional-options -fdeps-format=p1689r5 }

// { dg-prune-output "error: '-MF' and '-fdeps-file=' cannot share an output file" }
