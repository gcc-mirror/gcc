// P2295R6 - Support for UTF-8 as a portable source file encoding
// This test intentionally contains various byte sequences which are not valid UTF-8
// { dg-do preprocess }
// { dg-options "-finput-charset=UTF-8 -pedantic-errors -Wno-invalid-utf8" }

// a߿ࠀ퟿𐀀􏿿a		{ dg-bogus "invalid UTF-8 character" }
// a�a					{ dg-bogus "invalid UTF-8 character <80>" }
// a�a					{ dg-bogus "invalid UTF-8 character <bf>" }
// a�a					{ dg-bogus "invalid UTF-8 character <c0>" }
// a�a					{ dg-bogus "invalid UTF-8 character <c1>" }
// a�a					{ dg-bogus "invalid UTF-8 character <f5>" }
// a�a					{ dg-bogus "invalid UTF-8 character <ff>" }
// a�a					{ dg-bogus "invalid UTF-8 character <c2>" }
// a�a					{ dg-bogus "invalid UTF-8 character <e0>" }
// a���a				{ dg-bogus "invalid UTF-8 character <e0><80><bf>" }
// a���a				{ dg-bogus "invalid UTF-8 character <e0><9f><80>" }
// a�a					{ dg-bogus "invalid UTF-8 character <e0><bf>" }
// a�a					{ dg-bogus "invalid UTF-8 character <ec><80>" }
// a�a				{ dg-bogus "invalid UTF-8 character <ed><a0><80>" }
// a����a				{ dg-bogus "invalid UTF-8 character <f0><80><80><80>" }
// a����a				{ dg-bogus "invalid UTF-8 character <f0><8f><bf><bf>" }
// a����a				{ dg-bogus "invalid UTF-8 character <f4><90><80><80>" }
// a������a				{ dg-bogus "invalid UTF-8 character <fd><bf><bf><bf>" }
//					{ dg-bogus "invalid UTF-8 character <bf>" "" { target *-*-* } .-1 }
/* a߿ࠀ퟿𐀀􏿿a		{ dg-bogus "invalid UTF-8 character" } */
/* a�a					{ dg-bogus "invalid UTF-8 character <80>" } */
/* a�a					{ dg-bogus "invalid UTF-8 character <bf>" } */
/* a�a					{ dg-bogus "invalid UTF-8 character <c0>" } */
/* a�a					{ dg-bogus "invalid UTF-8 character <c1>" } */
/* a�a					{ dg-bogus "invalid UTF-8 character <f5>" } */
/* a�a					{ dg-bogus "invalid UTF-8 character <ff>" } */
/* a�a					{ dg-bogus "invalid UTF-8 character <c2>" } */
/* a�a					{ dg-bogus "invalid UTF-8 character <e0>" } */
/* a���a				{ dg-bogus "invalid UTF-8 character <e0><80><bf>" } */
/* a���a				{ dg-bogus "invalid UTF-8 character <e0><9f><80>" } */
/* a�a					{ dg-bogus "invalid UTF-8 character <e0><bf>" } */
/* a�a					{ dg-bogus "invalid UTF-8 character <ec><80>" } */
/* a�a				{ dg-bogus "invalid UTF-8 character <ed><a0><80>" } */
/* a����a				{ dg-bogus "invalid UTF-8 character <f0><80><80><80>" } */
/* a����a				{ dg-bogus "invalid UTF-8 character <f0><8f><bf><bf>" } */
/* a����a				{ dg-bogus "invalid UTF-8 character <f4><90><80><80>" } */
/* a������a				{ dg-bogus "invalid UTF-8 character <fd><bf><bf><bf>" } */
/*					{ dg-bogus "invalid UTF-8 character <bf>" "" { target *-*-* } .-1 } */
