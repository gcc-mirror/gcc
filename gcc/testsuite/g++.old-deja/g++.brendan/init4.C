// Build don't link: 
// GROUPS passed initialization
struct CharList { int i; };

const CharList& terminals = { 1 };// ERROR - .*
