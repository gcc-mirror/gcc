// P2295R6 - Support for UTF-8 as a portable source file encoding
// This test intentionally contains various byte sequences which are not valid UTF-8
// { dg-do preprocess }
// { dg-options "-finput-charset=UTF-8 -Winvalid-utf8" }

// aÂ€ß¿à €íŸ¿î€€ğ€€ô¿¿a		{ dg-bogus "invalid UTF-8 character" }
// a€a					{ dg-warning "invalid UTF-8 character '<80>'" }
// a¿a					{ dg-warning "invalid UTF-8 character '<bf>'" }
// aÀa					{ dg-warning "invalid UTF-8 character '<c0>'" }
// aÁa					{ dg-warning "invalid UTF-8 character '<c1>'" }
// aõa					{ dg-warning "invalid UTF-8 character '<f5>'" }
// aÿa					{ dg-warning "invalid UTF-8 character '<ff>'" }
// aÂa					{ dg-warning "invalid UTF-8 character '<c2>'" }
// aàa					{ dg-warning "invalid UTF-8 character '<e0>'" }
// aà€¿a				{ dg-warning "invalid UTF-8 character '<e0><80><bf>'" }
// aàŸ€a				{ dg-warning "invalid UTF-8 character '<e0><9f><80>'" }
// aà¿a					{ dg-warning "invalid UTF-8 character '<e0><bf>'" }
// aì€a					{ dg-warning "invalid UTF-8 character '<ec><80>'" }
// aí €a				{ dg-warning "invalid UTF-8 character '<ed><a0><80>'" }
// ağ€€€a				{ dg-warning "invalid UTF-8 character '<f0><80><80><80>'" }
// ağ¿¿a				{ dg-warning "invalid UTF-8 character '<f0><8f><bf><bf>'" }
// aô€€a				{ dg-warning "invalid UTF-8 character '<f4><90><80><80>'" }
// aı¿¿¿¿¿a				{ dg-warning "invalid UTF-8 character '<fd><bf><bf><bf>'" }
//					{ dg-warning "invalid UTF-8 character '<bf>'" "" { target *-*-* } .-1 }
/* aÂ€ß¿à €íŸ¿î€€ğ€€ô¿¿a		{ dg-bogus "invalid UTF-8 character" } */
/* a€a					{ dg-warning "invalid UTF-8 character '<80>'" } */
/* a¿a					{ dg-warning "invalid UTF-8 character '<bf>'" } */
/* aÀa					{ dg-warning "invalid UTF-8 character '<c0>'" } */
/* aÁa					{ dg-warning "invalid UTF-8 character '<c1>'" } */
/* aõa					{ dg-warning "invalid UTF-8 character '<f5>'" } */
/* aÿa					{ dg-warning "invalid UTF-8 character '<ff>'" } */
/* aÂa					{ dg-warning "invalid UTF-8 character '<c2>'" } */
/* aàa					{ dg-warning "invalid UTF-8 character '<e0>'" } */
/* aà€¿a				{ dg-warning "invalid UTF-8 character '<e0><80><bf>'" } */
/* aàŸ€a				{ dg-warning "invalid UTF-8 character '<e0><9f><80>'" } */
/* aà¿a					{ dg-warning "invalid UTF-8 character '<e0><bf>'" } */
/* aì€a					{ dg-warning "invalid UTF-8 character '<ec><80>'" } */
/* aí €a				{ dg-warning "invalid UTF-8 character '<ed><a0><80>'" } */
/* ağ€€€a				{ dg-warning "invalid UTF-8 character '<f0><80><80><80>'" } */
/* ağ¿¿a				{ dg-warning "invalid UTF-8 character '<f0><8f><bf><bf>'" } */
/* aô€€a				{ dg-warning "invalid UTF-8 character '<f4><90><80><80>'" } */
/* aı¿¿¿¿¿a				{ dg-warning "invalid UTF-8 character '<fd><bf><bf><bf>'" } */
/*					{ dg-warning "invalid UTF-8 character '<bf>'" "" { target *-*-* } .-1 } */
