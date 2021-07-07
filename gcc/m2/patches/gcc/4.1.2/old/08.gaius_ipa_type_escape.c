/* Copyright (C) 2010
 *               Free Software Foundation, Inc. */
/* This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GNU Modula-2 is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License along
with gm2; see the file COPYING.  If not, write to the Free Software
Foundation, 51 Franklin Street, Fifth Floor, Boston,
MA 02110-1301, USA. */
--- gcc-4.1.2-orig/gcc/ipa-type-escape.c	2005-09-25 06:28:01.000000000 +0100
+++ gcc-4.1.2/gcc/ipa-type-escape.c	2009-07-20 10:36:11.000000000 +0100
@@ -259,10 +259,33 @@
 static bool
 type_to_consider (tree type)
 {
+  int stackSize = 1;
+  int stackUsed = 1;
+  int oldSize;
+  int i;
+  tree *oldStack;
+  tree *stack = (tree *) alloca (sizeof (tree) * stackSize);
+  stack[0] = type;
+
+  /* return false if we detect a cyclic declaration of "array of pointer to ..." */
+
   /* Strip the *'s off.  */
   type = TYPE_MAIN_VARIANT (type);
-  while (POINTER_TYPE_P (type) || TREE_CODE (type) == ARRAY_TYPE)
+  while (POINTER_TYPE_P (type) || TREE_CODE (type) == ARRAY_TYPE) {
     type = TYPE_MAIN_VARIANT (TREE_TYPE (type));
+    for (i=0; i<stackUsed; i++)
+      if (stack[i] == type)
+	return false;
+    if (stackSize == stackUsed) {
+      oldSize = stackSize;
+      stackSize *= 2;
+      oldStack = stack;
+      stack = (tree *) alloca (sizeof (tree) * stackSize);
+      memcpy (stack, oldStack, oldSize);
+    }
+    stack[stackUsed] = type;
+    stackUsed++;
+  }
 
   switch (TREE_CODE (type))
     {
