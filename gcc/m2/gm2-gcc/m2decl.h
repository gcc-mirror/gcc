/* m2decl.h header file for m2decl.cc.

Copyright (C) 2012-2023 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaius@glam.ac.uk>.

This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Modula-2; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#if !defined(m2decl_h)

#define m2decl_h
#if defined(m2decl_c)
#if defined(__GNUG__)
#define EXTERN extern "C"
#else /* !__GNUG__.  */
#define EXTERN
#endif /* !__GNUG__.  */
#else /* !m2decl_c.  */
#if defined(__GNUG__)
#define EXTERN extern "C"
#else /* !__GNUG__.  */
#define EXTERN extern
#endif /* !__GNUG__.  */
#endif /* !m2decl_c.  */


EXTERN tree m2decl_DeclareM2linkStaticInitialization (location_t location,
						      int ScaffoldStatic);
EXTERN tree m2decl_DeclareM2linkForcedModuleInitOrder (location_t location,
						       const char *RuntimeOverride);
EXTERN tree m2decl_BuildPtrToTypeString (location_t location, const char *string, tree type);
EXTERN void m2decl_BuildModuleCtor (tree module_ctor);
EXTERN tree m2decl_DeclareModuleCtor (tree decl);
EXTERN tree m2decl_GetDeclContext (tree t);
EXTERN tree m2decl_BuildStringConstant (const char *string, int length);
EXTERN tree m2decl_BuildCStringConstant (const char *string, int length);
EXTERN tree m2decl_BuildConstLiteralNumber (location_t location,
					    const char *str,
                                            unsigned int base,
					    bool issueError);
EXTERN bool m2decl_DetermineSizeOfConstant (location_t location,
					    const char *str, unsigned int base,
                                            bool *needsLong,
                                            bool *needsUnsigned,
					    bool issueError);
EXTERN void m2decl_RememberVariables (tree l);

EXTERN tree m2decl_BuildEndFunctionDeclaration (
    location_t location_begin, location_t location_end, const char *name,
    tree returntype, bool isexternal, bool isnested, bool ispublic,
    bool isnoreturn);
EXTERN void m2decl_BuildStartFunctionDeclaration (bool uses_varargs);
EXTERN tree m2decl_BuildParameterDeclaration (location_t location, char *name,
                                              tree type, bool isreference);
EXTERN tree m2decl_DeclareKnownConstant (location_t location, tree type,
                                         tree value);
EXTERN tree m2decl_DeclareKnownVariable (location_t location, const char *name,
                                         tree type, bool exported, bool imported,
                                         bool istemporary, bool isglobal,
                                         tree scope, tree initial);

EXTERN tree m2decl_BuildStringConstantType (int length, const char *string,
                                            tree type);
EXTERN tree m2decl_BuildIntegerConstant (int value);

EXTERN int m2decl_GetBitsPerWord (void);
EXTERN int m2decl_GetBitsPerUnit (void);
EXTERN int m2decl_GetBitsPerInt (void);
EXTERN int m2decl_GetBitsPerBitset (void);

#undef EXTERN
#endif /* m2decl_h.  */
