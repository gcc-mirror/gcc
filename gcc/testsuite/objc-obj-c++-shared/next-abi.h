
#ifndef _OBJC_NEXT_ABI_H_
#define _OBJC_NEXT_ABI_H_
/* Produce a define that allows us to figure out what facilities are
   available for this gcc and OS combination.
*/

/* By default we do nothing - therefore ifdef NEXT_OBJC_USE_NEW_INTERFACE
 * is reliable for detecting 
 *  (a) versions of the compiler that are transitional to the new next ABI
 *  (b) versions of the target that require the new ABI.
 *
 * This applies for versions of OSX >= 10.5 (darwin9).
 *
 * A compiler capable of producing ObjC V2 ABI should define __OBJC2__
*/

#undef NEXT_OBJC_ABI_VERSION
#undef NEXT_OBJC_USE_NEW_INTERFACE

#ifdef __NEXT_RUNTIME__
#  if (MAC_OS_X_VERSION_MIN_REQUIRED >= MAC_OS_X_VERSION_10_5 || __OBJC2__)
    /* We have to use an updated interface for 32bit NeXT to avoid
     * 'deprecated' warnings. 
     * For 64bit NeXT the ABI is different (and the interfaces 'deprecated'
     * for 32bit have been removed).
    */
#    define NEXT_OBJC_USE_NEW_INTERFACE 1
#    if __OBJC2__ || __LP64__
       /* We have OBJC v2 ABI compiler, 
          (or, at least, the available NeXT runtime requires one) */
#      define NEXT_OBJC_ABI_VERSION 2
#    else
       /* We leave it open to define ABI 1 if and when we implement those 
        * extensions.
       */
#      define NEXT_OBJC_ABI_VERSION 0
#    endif
#  endif
#endif

#endif /* _OBJC_NEXT_ABI_H_ */
