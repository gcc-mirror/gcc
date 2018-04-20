/* { dg-do compile } */
/* { dg-options "-O3 -mcpu=power9" { target { powerpc*-*-* } } } */

package main
import (
	"go/ast"
	"go/parser"
	"go/token"
)
type testFuncs struct { }
func (t *testFuncs) load(filename, pkg string, doImport, seen *bool) {
	var testFileSet = token.NewFileSet()
	f, err := parser.ParseFile(testFileSet, filename, nil, parser.ParseComments)
	if err != nil { }
	for _, d := range f.Decls {
		n, ok := d.(*ast.FuncDecl)
		if !ok { }
		ptr := n.Type.Params.List[0].Type.(*ast.StarExpr)
		if sel := ptr.X.(*ast.SelectorExpr); sel.Sel.Name == "M" { }
	}
}
