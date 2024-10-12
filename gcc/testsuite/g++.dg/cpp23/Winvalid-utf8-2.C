// P2295R6 - Support for UTF-8 as a portable source file encoding
// This test intentionally contains various byte sequences which are not valid UTF-8
// { dg-do preprocess }
// { dg-options "-finput-charset=UTF-8 -pedantic" }

// aÂ€ß¿à €íŸ¿î€€ğ€€ô¿¿a		{ dg-bogus "invalid UTF-8 character" }
// a€a					{ dg-warning "invalid UTF-8 character '<80>'" "" { target c++23 } }
// a¿a					{ dg-warning "invalid UTF-8 character '<bf>'" "" { target c++23 } }
// aÀa					{ dg-warning "invalid UTF-8 character '<c0>'" "" { target c++23 } }
// aÁa					{ dg-warning "invalid UTF-8 character '<c1>'" "" { target c++23 } }
// aõa					{ dg-warning "invalid UTF-8 character '<f5>'" "" { target c++23 } }
// aÿa					{ dg-warning "invalid UTF-8 character '<ff>'" "" { target c++23 } }
// aÂa					{ dg-warning "invalid UTF-8 character '<c2>'" "" { target c++23 } }
// aàa					{ dg-warning "invalid UTF-8 character '<e0>'" "" { target c++23 } }
// aà€¿a				{ dg-warning "invalid UTF-8 character '<e0><80><bf>'" "" { target c++23 } }
// aàŸ€a				{ dg-warning "invalid UTF-8 character '<e0><9f><80>'" "" { target c++23 } }
// aà¿a					{ dg-warning "invalid UTF-8 character '<e0><bf>'" "" { target c++23 } }
// aì€a					{ dg-warning "invalid UTF-8 character '<ec><80>'" "" { target c++23 } }
// aí €a				{ dg-warning "invalid UTF-8 character '<ed><a0><80>'" "" { target c++23 } }
// ağ€€€a				{ dg-warning "invalid UTF-8 character '<f0><80><80><80>'" "" { target c++23 } }
// ağ¿¿a				{ dg-warning "invalid UTF-8 character '<f0><8f><bf><bf>'" "" { target c++23 } }
// aô€€a				{ dg-warning "invalid UTF-8 character '<f4><90><80><80>'" "" { target c++23 } }
// aı¿¿¿¿¿a				{ dg-warning "invalid UTF-8 character '<fd><bf><bf><bf>'" "" { target c++23 } }
//					{ dg-warning "invalid UTF-8 character '<bf>'" "" { target c++23 } .-1 }
/* aÂ€ß¿à €íŸ¿î€€ğ€€ô¿¿a		{ dg-bogus "invalid UTF-8 character" } */
/* a€a					{ dg-warning "invalid UTF-8 character '<80>'" "" { target c++23 } } */
/* a¿a					{ dg-warning "invalid UTF-8 character '<bf>'" "" { target c++23 } } */
/* aÀa					{ dg-warning "invalid UTF-8 character '<c0>'" "" { target c++23 } } */
/* aÁa					{ dg-warning "invalid UTF-8 character '<c1>'" "" { target c++23 } } */
/* aõa					{ dg-warning "invalid UTF-8 character '<f5>'" "" { target c++23 } } */
/* aÿa					{ dg-warning "invalid UTF-8 character '<ff>'" "" { target c++23 } } */
/* aÂa					{ dg-warning "invalid UTF-8 character '<c2>'" "" { target c++23 } } */
/* aàa					{ dg-warning "invalid UTF-8 character '<e0>'" "" { target c++23 } } */
/* aà€¿a				{ dg-warning "invalid UTF-8 character '<e0><80><bf>'" "" { target c++23 } } */
/* aàŸ€a				{ dg-warning "invalid UTF-8 character '<e0><9f><80>'" "" { target c++23 } } */
/* aà¿a					{ dg-warning "invalid UTF-8 character '<e0><bf>'" "" { target c++23 } } */
/* aì€a					{ dg-warning "invalid UTF-8 character '<ec><80>'" "" { target c++23 } } */
/* aí €a				{ dg-warning "invalid UTF-8 character '<ed><a0><80>'" "" { target c++23 } } */
/* ağ€€€a				{ dg-warning "invalid UTF-8 character '<f0><80><80><80>'" "" { target c++23 } } */
/* ağ¿¿a				{ dg-warning "invalid UTF-8 character '<f0><8f><bf><bf>'" "" { target c++23 } } */
/* aô€€a				{ dg-warning "invalid UTF-8 character '<f4><90><80><80>'" "" { target c++23 } } */
/* aı¿¿¿¿¿a				{ dg-warning "invalid UTF-8 character '<fd><bf><bf><bf>'" "" { target c++23 } } */
/*					{ dg-warning "invalid UTF-8 character '<bf>'" "" { target c++23 } .-1 } */
